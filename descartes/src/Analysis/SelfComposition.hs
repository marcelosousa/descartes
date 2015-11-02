{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.SelfComposition
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.SelfComposition where

import Analysis.Axioms
import Analysis.Engine
import Analysis.Hoare
import Analysis.Invariant
import Analysis.Properties
import Analysis.Util
import Analysis.Types

import Control.Monad.State.Strict
import Data.Map (Map)
import Data.Maybe

import Language.Java.Pretty
import Language.Java.Syntax

import System.IO.Unsafe
import Z3.Monad hiding (Params)

import qualified Data.Map as M
import qualified Debug.Trace as T

verifyWithSelf :: ClassMap -> [Comparator] -> Prop -> Z3 (Result, Maybe String)
verifyWithSelf classMap comps prop = do
  (objSort, pars, res, fields) <- prelude classMap comps
  (pre, post) <- trace ("after prelude:" ++ show (objSort, pars, res, fields)) $ prop (pars, res, fields)
  (fields', axioms) <- addAxioms objSort fields
  let blocks = zip [0..] $ getBlocks comps
  iSSAMap <- getInitialSSAMap
  let iEnv = Env objSort pars res fields' iSSAMap axioms pre post False False False
  pres <- mapM (\p -> evalStateT (selfcomposition p) iEnv) blocks
--  pres <- mapM (selfcomposition (objSort, pars, res, fields', iSSAMap, axioms, pre)) blocks
  pre' <- mkAnd pres
  preStr  <- astToString pre'
  (res, mmodel) <- T.trace ("\n-----------------\nFinal Pre:\n" ++ preStr) $ local $ helper axioms pre' post
  case res of 
    Unsat -> return (Unsat, Nothing)
    Sat -> do
      str <- showModel $ fromJust mmodel
      return (Sat, Just str)
            
-- self-composition
selfcomposition :: (Int, Block) -> EnvOp AST
selfcomposition (pid,Block l) = do
 env@Env{..} <- get
 case l of
  [] -> return _pre
  (bstmt:rest) -> do
   preStr  <- lift $ astToString _pre
   --let k = T.trace ("\n-----------------\nAnalyser State:\nStatement: " ++ prettyPrint bstmt ++ "\nPrecondition:\n" ++ preStr) $ unsafePerformIO $ getChar
   case bstmt of
    BlockStmt stmt -> analyser_stmt stmt (pid, Block rest)
    LocalVars mods ty vars -> do
     sort <- lift $ processType ty    
     (nssamap, npre) <- 
       lift $ foldM (\(ssamap', pre') v -> 
         processNewVar (_objSort,_params,_res,_fields,ssamap',pre') sort v 1) (_ssamap, _pre) vars
     updatePre npre
     updateSSAMap nssamap
     selfcomposition (pid, Block rest)

analyser_stmt :: Stmt -> (Int,Block) -> EnvOp AST
analyser_stmt stmt (pid, Block r1) =
 case stmt of
  StmtBlock (Block block) -> selfcomposition (pid, Block (block ++ r1))
  Assume expr -> do
   assume expr
   selfcomposition (pid, Block r1)
  Return mexpr -> do
   ret pid mexpr
   env@Env{..} <- get
   return _pre
--   selfcomposition (pid, Block r1)
  IfThen cond s1 -> do
   let ifthenelse = IfThenElse cond s1 (StmtBlock (Block []))
   analyser_stmt ifthenelse (pid, Block r1)
  IfThenElse cond s1 s2 -> analyse_conditional pid r1 cond s1 s2 
  ExpStmt expr -> analyse_exp pid (pid,Block r1) expr
  While _cond _body -> analyse_loop pid r1 _cond _body

-- Analyse Expressions
analyse_exp :: Int -> (Int, Block) -> Exp -> EnvOp AST
analyse_exp pid rest _exp =
 case _exp of
  MethodInv minv -> do 
   method_call minv
   selfcomposition rest
  Assign lhs aOp rhs -> do
   assign _exp lhs aOp rhs
   selfcomposition rest 
  PostIncrement lhs -> do
   postOp _exp lhs Add "PostIncrement"
   selfcomposition rest
  PostDecrement lhs -> do
   postOp _exp lhs Sub "PostDecrement"
   selfcomposition rest

-- Analyse If Then Else
analyse_conditional :: Int -> [BlockStmt] -> Exp -> Stmt -> Stmt -> EnvOp AST
analyse_conditional pid r1 cond s1 s2 =
 if cond == Nondet
 then do
  resThen <- selfcomposition (pid, Block (BlockStmt s1:r1))
  resElse <- selfcomposition (pid, Block (BlockStmt s2:r1))
  lift $ mkOr [resThen, resElse]
 else do
  env@Env{..} <- get
  condSmt <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) cond
  -- then branch
  preThen <- lift $ mkAnd [_pre, condSmt]
  resThen <- analyse_branch preThen s1
  -- else branch
  put env
  ncondSmt <- lift $ mkNot condSmt
  preElse <- lift $ mkAnd [_pre, ncondSmt]
  resElse <- analyse_branch preElse s2
  lift $ mkOr [resThen, resElse]
 where
   analyse_branch phi branch = do
    env@Env{..} <- get
    updatePre phi
    let r = (pid, Block (BlockStmt branch:r1))
    selfcomposition r

-- Analyse Loops
analyse_loop :: Int -> [BlockStmt] -> Exp -> Stmt -> EnvOp AST
analyse_loop pid r1 _cond _body = do
 let bstmt = BlockStmt $ While _cond _body
 env@Env{..} <- get
 invs <- guessInvariants (pid+1) _cond
 analyse_loop_w_inv invs
 where
   analyse_loop_w_inv [] = error "analyse_loop failed"
   analyse_loop_w_inv (inv:is) = do
    it_res <- _analyse_loop pid _cond _body inv
    if it_res
    then do
     env@Env{..} <- get
     pre <- lift $ mkAnd [inv,_pre]
     updatePre pre
     selfcomposition (pid,Block r1)
    else analyse_loop_w_inv is
   
--
_analyse_loop :: Int -> Exp -> Stmt -> AST -> EnvOp Bool
_analyse_loop pid _cond _body inv = do
 invStr  <- lift $ astToString inv
 env@Env{..} <- get --T.trace ("Invariant:\n" ++ invStr) $ get
 (checkPre,_) <- lift $ local $ helper _axioms _pre inv
 case checkPre of
  Unsat -> do
   condAst <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) _cond
   ncondAst <- lift $ mkNot condAst
   (checkInv,_) <- lift $ mkAnd [inv,ncondAst] >>= \npre -> local $ helper _axioms npre inv
   case checkInv of
    Unsat -> do
     pre <- lift $ mkAnd [inv,condAst]
     let s = (pid, Block [BlockStmt _body])
     updatePre pre
     updatePost inv
     pre' <- selfcomposition s
     (bodyCheck,_) <- lift $ local $ helper _axioms pre' inv
     case bodyCheck of
      Unsat -> return True
      Sat -> do
       put env
       return False -- {inv && pre} body {inv} failed
    Sat -> return False -- inv && not_cond =/=> inv
  Sat -> return False -- pre =/=> inv

{-
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
        inv <- undefined --guessInvariants (objSort, pars, res, fields, ssamap) (pid+1) _cond pre
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
-}    