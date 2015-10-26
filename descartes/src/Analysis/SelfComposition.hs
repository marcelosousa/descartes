-------------------------------------------------------------------------------
-- Module    :  Analysis.SelfComposition
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.SelfComposition where

import Z3.Monad

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State.Strict

import Language.Java.Syntax
import Language.Java.Pretty

import Analysis.Types
import Analysis.Util
import Analysis.Properties
import Analysis.Axioms
import Analysis.Engine
import Analysis.Invariant


verifyWithSelf :: ClassMap -> [Comparator] -> Prop -> Z3 (Result, Maybe String)
verifyWithSelf classMap comps prop = do
  (objSort, pars, res, fields) <- prelude classMap comps
  (pre, post) <- trace ("after prelude:" ++ show (objSort, pars, res, fields)) $ prop (pars, res, fields)
  (fields', axioms) <- addAxioms objSort fields
  let blocks = zip [0..] $ getBlocks comps
  iSSAMap <- getInitialSSAMap
  pres <- mapM (selfcomposition (objSort, pars, res, fields', iSSAMap, axioms, pre)) blocks
  pre' <- mkAnd pres
  preStr  <- astToString pre'
  (res, mmodel) <- trace ("\n-----------------\nFinal Pre:\n" ++ preStr) $ local $ helper axioms pre' post
  case res of 
    Unsat -> return (Unsat, Nothing)
    Sat -> do
      str <- showModel $ fromJust mmodel
      return (Sat, Just str)
            
-- self-composition
selfcomposition :: (Sort, Args, [AST], Fields, SSAMap, AST, AST) -> (Int, Block) -> Z3 AST
selfcomposition env@(objSort, pars, res, fields, ssamap, axioms, pre) (pid,Block []) = return pre
selfcomposition env@(objSort, pars, res, fields, ssamap, axioms, pre) (pid,Block (bstmt:r1)) = do
  preStr  <- astToString pre
  --let k = T.trace ("\n-----------------\nAnalyser State:\nStatement: " ++ prettyPrint bstmt ++ "\nPrecondition:\n" ++ preStr) $ unsafePerformIO $ getChar
  case bstmt of
    BlockStmt stmt -> case stmt of
      StmtBlock (Block block) -> selfcomposition env (pid, Block (block ++ r1))
      Assume expr -> do
        exprEnc <- processExp (objSort, pars, res, fields, ssamap) expr
        nPre <- mkAnd [pre,exprEnc]
        selfcomposition (objSort, pars, res, fields, ssamap, axioms, nPre) (pid, Block r1)
      Return Nothing -> error "self-composition: return Nothing"
      Return (Just expr) -> trace ("processing return of pid " ++ show pid ++ " " ++ show stmt) $ do
        exprPsi <- processExp (objSort, pars, res, fields, ssamap) expr
        let resPid = res !! pid  
        r <- mkEq resPid exprPsi
        nPre <- mkAnd [pre,r]
        return nPre
      IfThen cond s1 -> do
        condSmt <- processExp (objSort, pars, res, fields, ssamap) cond
        -- then branch
        preThen <- trace ("processing THEN branch") $ mkAnd [pre, condSmt]
        resThen <- selfcomposition (objSort, pars, res, fields, ssamap, axioms, preThen) (pid, Block (BlockStmt s1:r1))
        -- else branch
        ncondSmt <- trace ("processing ELSE branch") $ mkNot condSmt
        preElse <- mkAnd [pre, ncondSmt]
        resElse <- selfcomposition (objSort, pars, res, fields, ssamap, axioms, preElse) (pid, Block r1)
        mkOr [resThen, resElse]
      IfThenElse Nondet s1 s2 -> do 
        resThen <- selfcomposition (objSort, pars, res, fields, ssamap, axioms, pre) (pid, Block (BlockStmt s1:r1))                
        resElse <- selfcomposition (objSort, pars, res, fields, ssamap, axioms, pre) (pid, Block (BlockStmt s2:r1))
        mkOr [resThen, resElse]
      IfThenElse cond s1 s2 -> trace ("processing conditional " ++ show cond) $ do
        condSmt <- processExp (objSort, pars, res, fields, ssamap) cond
        -- then branch
        preThen <- trace ("processing THEN branch") $ mkAnd [pre, condSmt]
        resThen <- selfcomposition (objSort, pars, res, fields, ssamap, axioms, preThen) (pid, Block (BlockStmt s1:r1))
        -- else branch
        ncondSmt <- trace ("processing ELSE branch") $ mkNot condSmt
        preElse <- mkAnd [pre, ncondSmt]
        resElse <- selfcomposition (objSort, pars, res, fields, ssamap, axioms, preElse) (pid, Block (BlockStmt s2:r1))
        mkOr [resThen, resElse]
      ExpStmt (MethodInv (MethodCall (Name [Ident "assume"]) [expr])) -> do 
        expAST <- processExp (objSort, pars, res, fields, ssamap) expr
        npre <- mkAnd [pre,expAST]
        selfcomposition (objSort, pars, res, fields, ssamap, axioms, npre) (pid, Block r1)
      ExpStmt (Assign lhs aOp rhs) -> do
        rhsAst <- processExp (objSort, pars, res, fields, ssamap) rhs
        case lhs of
          NameLhs (Name [ident@(Ident str)]) -> do
            let (plhsAST,sort, i) = safeLookup "Assign" ident ssamap
                cstr = str ++ show i
                ni = i+1
                nstr = str ++ show ni
            sym <- mkStringSymbol nstr
            var <- mkFreshFuncDecl nstr [] sort
            astVar <- mkApp var []
            let nssamap = M.insert ident (astVar, sort, ni) ssamap
            ass <- processAssign astVar aOp rhsAst plhsAST
            npre <- mkAnd [pre, ass]
            -- post' <- replaceVariable cstr var post: need to solve this!
            selfcomposition (objSort, pars, res, fields, nssamap, axioms, npre) (pid, Block r1)
          _ -> error $ "Assign " ++ show stmt ++ " not supported"
      ExpStmt (PostIncrement lhs) -> do
        rhsAst <- processExp (objSort, pars, res, fields, ssamap) (BinOp lhs Add (Lit $ Int 1))
        case lhs of
          ExpName (Name [ident@(Ident str)]) -> do
            let (plhsAST,sort, i) = safeLookup "Assign" ident ssamap
                cstr = str ++ show i
                ni = i+1
                nstr = str ++ show ni
            sym <- mkStringSymbol nstr
            var <- mkFreshFuncDecl nstr [] sort
            astVar <- mkApp var []
            let nssamap = M.insert ident (astVar, sort, ni) ssamap
            ass <- processAssign astVar EqualA rhsAst plhsAST
            npre <- mkAnd [pre, ass]
            -- post' <- replaceVariable cstr var post: need to solve this!
            selfcomposition (objSort, pars, res, fields, nssamap, axioms, npre) (pid, Block r1)
          _ -> error $ "PostIncrement " ++ show stmt ++ " not supported"
      ExpStmt (PostDecrement lhs) -> do
        rhsAst <- processExp (objSort, pars, res, fields, ssamap) (BinOp lhs Sub (Lit $ Int 1))
        case lhs of
          ExpName (Name [ident@(Ident str)]) -> do
            let (plhsAST,sort, i) = safeLookup "Assign" ident ssamap
                cstr = str ++ show i
                ni = i+1
                nstr = str ++ show ni
            sym <- mkStringSymbol nstr
            var <- mkFreshFuncDecl nstr [] sort
            astVar <- mkApp var []
            let nssamap = M.insert ident (astVar, sort, ni) ssamap
            ass <- processAssign astVar EqualA rhsAst plhsAST
            npre <- mkAnd [pre, ass]
            -- post' <- replaceVariable cstr var post: need to solve this!
            selfcomposition (objSort, pars, res, fields, nssamap, axioms, npre) (pid, Block r1)
          _ -> error $ "PostIncrement " ++ show stmt ++ " not supported"
      While _cond _body -> trace ("\nProcessing While loop from PID" ++ show pid ++"\n") $ do
        inv <- guessInvariant (objSort, pars, res, fields, ssamap) (pid+1) _cond pre
        checkInv <- local $ helper axioms pre inv
        invStr <- astToString inv
        preStr <- astToString pre
        --let k = T.trace ("\nPrecondition:\n"++ preStr ++ "\nInvariant:\n" ++ invStr ++ "\npid: " ++ show pid) $ unsafePerformIO $ getChar
        case checkInv of
          (Unsat,_) -> do
            condAst <- processExp (objSort, pars, res, fields, ssamap) _cond
            ncondAst <- mkNot condAst
            checkInv' <- mkAnd [inv, ncondAst] >>= \npre -> local $ helper axioms npre inv
            case checkInv' of
              (Unsat,_) -> do
                nPre <- mkAnd [inv, condAst]
                let s = (pid, Block [BlockStmt _body])
                pre' <- local $ selfcomposition (objSort, pars, res, fields, ssamap, axioms, nPre) s
                bodyCheck <- local $ helper axioms pre' inv
                case bodyCheck of
                  (Unsat,_) -> do 
                    rPre <- mkAnd [inv, pre]
                    selfcomposition (objSort, pars, res, fields, ssamap, axioms, inv) (pid, Block r1)
                  _ -> error $ "bodyCheck: SAT"
              _ -> error $ "lastCheck failed"
          _ -> error "precondition does not imply the invariant"
      _ -> error $ "not supported: " ++ show stmt
    LocalVars mods ty vars -> do
      sort <- processType ty
      (nssamap, npre) <- foldM (\(ssamap', pre') v -> processNewVar (objSort, pars, res, fields, ssamap', pre') sort v 1) (ssamap, pre) vars
--      let nssamap = foldl (\m (ident,ast, _) -> M.insert ident (ast, sort, 1) m) ssamap idAst
      selfcomposition (objSort, pars, res, fields, nssamap, axioms, npre) (pid, Block r1)
    _ -> error "analyser: bstmt is not a BlockStmt"
    