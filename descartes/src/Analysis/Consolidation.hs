-------------------------------------------------------------------------------
-- Module    :  Analysis.Consolidation
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Consolidation where

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

import System.IO.Unsafe
import qualified Debug.Trace as T

trace a b = b
--trace = T.trace

verify :: Bool -> ClassMap -> [Comparator] -> Prop -> Z3 (Result, Maybe String)
verify opt classMap _comps prop = do
  let comps = _comps -- map rewrite _comps
  (objSort, pars, res, fields) <- prelude classMap comps
  (pre, post) <- trace ("after prelude:" ++ show (objSort, pars, res, fields)) $ prop (pars, res, fields)
  (fields', axioms) <- addAxioms objSort fields
  let blocks = zip [0..] $ getBlocks comps
  iSSAMap <- getInitialSSAMap
  (res, mmodel) <- analyser opt (objSort, pars, res, fields', iSSAMap, axioms, pre, post) blocks
  case res of 
    Unsat -> return (Unsat, Nothing)
    Sat -> do
      str <- showModel $ fromJust mmodel
      return (Sat, Just str)
        
-- strongest post condition
analyser :: Bool -> (Sort, Args, [AST], Fields, SSAMap, AST, AST, AST) -> [(Int, Block)] -> Z3 (Result, Maybe Model)
analyser opt (objSort, pars, res, fields, ssamap, axioms, pre, post) [] = do 
    --preStr  <- astToString pre
    --postStr <- astToString post
    --let k = trace ("Analyser Leaf:\nPrecondition:\n" ++ preStr ++ "\nPostcondition: " ++ postStr) $ unsafePerformIO $ getChar
    local $ helper axioms pre post
analyser opt env ((pid,Block []):rest) = analyser opt env rest
analyser opt env@(objSort, pars, res, fields, ssamap, axioms, pre, post) ((pid,Block (bstmt:r1)):rest) = do
    preStr  <- astToString pre
    postStr <- astToString post
    --let k = T.trace ("\n-----------------\nAnalyser State:\nStatement: " ++ prettyPrint bstmt ++ "\nPrecondition:\n" ++ preStr ++ "\nPostcondition: " ++ postStr) $ unsafePerformIO $ getChar
    case bstmt of
        BlockStmt stmt -> case stmt of
            StmtBlock (Block block) -> analyser opt env ((pid, Block (block ++ r1)):rest)
            Assume expr -> do
                exprEnc <- processExp (objSort, pars, res, fields, ssamap) expr
                nPre <- mkAnd [pre,exprEnc]
                analyser opt (objSort, pars, res, fields, ssamap, axioms, nPre, post) ((pid, Block r1):rest)
            Return Nothing -> error "analyser: return Nothing"
            Return (Just expr) -> trace ("processing return of pid " ++ show pid ++ " " ++ show stmt) $ do
                exprPsi <- processExp (objSort, pars, res, fields, ssamap) expr
                let resPid = res !! pid  
                r <- mkEq resPid exprPsi
                nPre <- mkAnd [pre,r]
                if opt
                then do
                  test <- local $ helper axioms nPre post
                  case fst test of
                      Unsat -> trace ("Optimizer: Pruned Path") $ return test
                      _ -> analyser opt (objSort, pars, res, fields, ssamap, axioms, nPre, post) rest
                else analyser opt (objSort, pars, res, fields, ssamap, axioms, nPre, post) rest
            IfThen cond s1 -> do
                condSmt <- processExp (objSort, pars, res, fields, ssamap) cond
                -- then branch
                preThen <- trace ("processing THEN branch") $ mkAnd [pre, condSmt]
                -- if opt then call solver, else continue
                resThen <- do
                    if opt
                    then do 
                        push 
                        assert preThen
                        cThen <- check
                        pop 1
                        case cThen of
                            Unsat -> trace ("preThen becomes false") $ return (Unsat,Nothing)
                            _ -> analyser opt (objSort, pars, res, fields, ssamap, axioms, preThen, post) ((pid, Block (BlockStmt s1:r1)):rest)
                    else analyser opt (objSort, pars, res, fields, ssamap, axioms, preThen, post) ((pid, Block (BlockStmt s1:r1)):rest)
                -- else branch
                ncondSmt <- trace ("processing ELSE branch") $ mkNot condSmt
                preElse <- mkAnd [pre, ncondSmt]
                resElse <- do
                    if opt 
                    then do 
                        push
                        assert preElse
                        cElse <- check
                        pop 1
                        case cElse of
                            Unsat -> trace ("preElse becomes false") $ return (Unsat,Nothing)
                            _ -> analyser opt (objSort, pars, res, fields, ssamap, axioms, preElse, post) ((pid, Block r1):rest)
                    else analyser opt (objSort, pars, res, fields, ssamap, axioms, preElse, post) ((pid, Block r1):rest)
                trace ("IfThen " ++ show (fst resThen, fst resElse)) $ combine resThen resElse
            IfThenElse Nondet s1 s2 -> do 
                resThen <- analyser opt (objSort, pars, res, fields, ssamap, axioms, pre, post) ((pid, Block (BlockStmt s1:r1)):rest)                
                resElse <- analyser opt (objSort, pars, res, fields, ssamap, axioms, pre, post) ((pid, Block (BlockStmt s2:r1)):rest)
                trace ("NonDet IfThenElse " ++ show (fst resThen, fst resElse)) $ combine resThen resElse                
            IfThenElse cond s1 s2 -> trace ("processing conditional " ++ show cond) $ do
                condSmt <- processExp (objSort, pars, res, fields, ssamap) cond
                -- then branch
                preThen <- trace ("processing THEN branch") $ mkAnd [pre, condSmt]
                resThen <- do
                    if opt
                    then do 
                        push 
                        assert preThen
                        cThen <- check
                        pop 1
                        case cThen of
                            Unsat -> trace ("preThen becomes false") $ return (Unsat,Nothing)
                            _ -> analyser opt (objSort, pars, res, fields, ssamap, axioms, preThen, post) ((pid, Block (BlockStmt s1:r1)):rest)
                    else analyser opt (objSort, pars, res, fields, ssamap, axioms, preThen, post) ((pid, Block (BlockStmt s1:r1)):rest)
                -- else branch
                ncondSmt <- trace ("processing ELSE branch") $ mkNot condSmt
                preElse <- mkAnd [pre, ncondSmt]
                resElse <- do
                    if opt
                    then do
                        push
                        assert preElse
                        cElse <- check
                        pop 1
                        case cElse of
                            Unsat -> trace ("preElse becomes false") $ return (Unsat,Nothing)
                            _ -> analyser opt (objSort, pars, res, fields, ssamap, axioms, preElse, post) ((pid, Block (BlockStmt s2:r1)):rest)
                        else analyser opt (objSort, pars, res, fields, ssamap, axioms, preElse, post) ((pid, Block (BlockStmt s2:r1)):rest)
                trace ("IfThenElse " ++ show (fst resThen, fst resElse)) $ combine resThen resElse
            ExpStmt (MethodInv (MethodCall (Name [Ident "assume"]) [expr])) -> do 
                expAST <- processExp (objSort, pars, res, fields, ssamap) expr
                npre <- mkAnd [pre,expAST]
                analyser opt (objSort, pars, res, fields, ssamap, axioms, npre, post) ((pid, Block r1):rest)
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
                        post' <- replaceVariable cstr var post
                        analyser opt (objSort, pars, res, fields, nssamap, axioms, npre, post') ((pid, Block r1):rest)
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
                        post' <- replaceVariable cstr var post
                        analyser opt (objSort, pars, res, fields, nssamap, axioms, npre, post') ((pid, Block r1):rest)
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
                        post' <- replaceVariable cstr var post
                        analyser opt (objSort, pars, res, fields, nssamap, axioms, npre, post') ((pid, Block r1):rest)
                    _ -> error $ "PostIncrement " ++ show stmt ++ " not supported"
            While _cond _body -> trace ("\nProcessing While loop from PID" ++ show pid ++"\n") $ do
                let 
                --if all isLoop rest
                --then applyFusion env ((pid,Block (bstmt:r1)):rest)
                --else analyser env $ (rest ++ [(pid,Block (bstmt:r1))])
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
                                let s = [(pid, Block [BlockStmt _body])] 
                                bodyCheck <- local $ analyser opt (objSort, pars, res, fields, ssamap, axioms, nPre, inv) s
                                case bodyCheck of
                                    (Unsat,_) -> do 
                                        rPre <- mkAnd [inv, pre]
                                        analyser opt (objSort, pars, res, fields, ssamap, axioms, inv, post) ((pid, Block r1):rest)
                                    _ -> error $ "bodyCheck: SAT"
                            _ -> error $ "lastCheck failed"
                    _ -> error "precondition does not imply the invariant"
            _ -> error $ "not supported: " ++ show stmt
        LocalVars mods ty vars -> do
            sort <- processType ty
            (nssamap, npre) <- foldM (\(ssamap', pre') v -> processNewVar (objSort, pars, res, fields, ssamap', pre') sort v 1) (ssamap, pre) vars
--            let nssamap = foldl (\m (ident,ast, _) -> M.insert ident (ast, sort, 1) m) ssamap idAst
            analyser opt (objSort, pars, res, fields, nssamap, axioms, npre, post) ((pid, Block r1):rest)
        _ -> error "analyser: bstmt is not a BlockStmt"




    
combine :: (Result, Maybe Model) -> (Result, Maybe Model) -> Z3 (Result, Maybe Model)
combine (Unsat,_) (Unsat,_) = return (Unsat, Nothing)
combine (Unsat,_) res   = return res
combine res   _     = return res

applyFusion :: Bool -> (Sort, Args, [AST], Fields, SSAMap, AST, AST, AST) -> [(Int, Block)] -> Z3 (Result, Maybe Model)
applyFusion opt env@(objSort, pars, res, fields, ssamap, axioms, pre, post) list = trace ("applying Fusion!!") $ do
    let (loops, rest) = unzip $ map takeHead list
        (_conds, bodies) = unzip $ map splitLoop loops
        (pids, conds) = unzip _conds
    astApps <- mapM (makeApp ssamap) pids
    let (asts, apps) = unzip astApps
    inv' <- mkExistsConst [] apps pre
    -- equality constraints between the loop counter iterations: i1 = i2 and i1 = i3 ...
    eqs <- mapM (\c -> mkEq (head asts) c) (tail asts)
    eqInv <- mkAnd eqs
    -- the candidate invariant
    inv <- mkAnd [inv',eqInv]
    checkInv <- local $ helper axioms pre inv
    invStr <- astToString inv
    preStr <- astToString pre
    --let k = T.trace ("\nPrecondition:\n"++ preStr ++ "\nInvariant:\n" ++ invStr) $ unsafePerformIO $ getChar
    case checkInv of
        (Unsat,_) -> do
            -- the new precondition inside the loop
            condsAsts <- mapM (processExp (objSort, pars, res, fields, ssamap)) conds 
            ncondsAsts <- mapM mkNot condsAsts
            bodyPre <- mkAnd $ inv:condsAsts
            bodyCheck <- local $ analyser opt (objSort, pars, res, fields, ssamap, axioms, bodyPre, inv) bodies
            case bodyCheck of
                (Unsat,_) -> do
                    condsNAst <- mkAnd condsAsts >>= mkNot
                    nPre <- mkAnd [inv,condsNAst]
                    ncondAst <- mkAnd ncondsAsts
                    lastCheck <- local $ helper axioms nPre ncondAst
                    case lastCheck of
                        (Unsat,_) -> analyser opt (objSort, pars, res, fields, ssamap, axioms, nPre, post) rest
                        _ -> error "lastCheck failed"
                _ -> error "couldnt prove the loop bodies with invariant"
        _ -> error "precondition does not imply the invariant"

-- Begin Fusion Utility Functions
isLoop :: (Int, Block) -> Bool
isLoop (_, Block ((BlockStmt (While _ _)):rest)) = True
isLoop _ = False
        
takeHead :: (Int, Block) -> ((Int, Stmt), (Int,Block))
takeHead (pid, Block []) = error "takeHead"
takeHead (pid, Block ((BlockStmt b):rest)) = ((pid,b), (pid, Block rest))

splitLoop :: (Int, Stmt) -> ((Int, Exp), (Int, Block))
splitLoop (pid, While cond body) = 
  case body of
    StmtBlock block -> ((pid, cond), (pid,block))
    _ -> error "splitLoop constructing block out of loop body"
splitLoop _ = error "splitLoop"

makeApp :: SSAMap -> Int -> Z3 (AST,App)
makeApp ssamap pid = do
  let i = Ident $ "i" ++ show (pid+1)
      (iAST,_,_)  = safeLookup "guessInvariant: i" i ssamap
  iApp <- toApp iAST
  return (iAST,iApp)
-- End Fusion Utility Functions