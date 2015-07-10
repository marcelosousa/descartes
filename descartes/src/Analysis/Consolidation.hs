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

type ConState = AState Comparator
type SSAMap = Map Ident (AST, Sort, Int)

getInitialSSAMap :: Z3 SSAMap
getInitialSSAMap = do
    iSort <- mkIntSort
    fn <- mkFreshFuncDecl "null" [] iSort
    ast <- mkApp fn []
    return $ M.singleton (Ident "null") (ast, iSort, 0)


-- result: (ObjectType, Parameters, Results)
prelude :: ClassMap -> [Comparator] -> Z3 (Sort, Args, [AST], Fields)
prelude classMap comps = do
    let arity = length comps
    if arity == 0
    then error "init: empty comparators"
    else do
        let objType = getObjectType $ head comps
        objSort <- mkObjectSort objType
        let objFields = fields $ safeLookup "prelude" objType classMap
            fieldNames = getFieldNames objType classMap
--        fields <- mapM (\f -> mkField f objSort) fieldNames
        let --fields' = foldl (\r (k,v) -> M.insert (Ident k) v r) M.empty $ zip fieldNames fields
            parsId = concatMap (getParIdents . getParameters) comps
        -- methods
        fields' <- foldM (mkAttribute objSort) M.empty objFields
        pars <- mapM (\par -> mkFreshConst par objSort) parsId
        let pars' = foldl (\r (k,v) -> M.insert (Ident k) v r) M.empty $ zip parsId pars
        intSort <- mkIntSort
        res <- mapM (\idx -> mkFreshConst ("res"++show idx) intSort) [1..arity]
        return (objSort, pars', res, fields')

mkAttribute :: Sort -> Fields -> MemberDecl -> Z3 Fields
mkAttribute objSort m mDecl = case mDecl of
    FieldDecl  mods ty vardecls -> do
        retSort <- processType ty
        foldM (\nm vardecl -> mkField nm vardecl objSort retSort) m vardecls 
    MethodDecl mods ty (Just rty) (Ident name) pars exTy (MethodBody Nothing) -> trace ("processing " ++ name ++ " " ++ show (rty,pars) ) $ do
        retSort <- processType rty
        i <- mkIntSort
        parsSort <- mapM processParam pars
        fn <- mkFreshFuncDecl name (objSort:parsSort) retSort
        return $ M.insert (Ident name) fn m

processParam :: FormalParam -> Z3 Sort
processParam (FormalParam mods ty _ _) = processType ty 

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

isLoop :: (Int, Block) -> Bool
isLoop (_, Block ((BlockStmt (While _ _)):rest)) = True
isLoop _ = False

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
                                let s = [(pid, Block [BlockStmt _body])] 
                                bodyCheck <- local $ analyser True (objSort, pars, res, fields, ssamap, axioms, nPre, inv) s
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
--            let nssamap = foldl (\m (ident,ast, _) -> M.insert ident (ast, sort, 1) m) ssamap idAst
            selfcomposition (objSort, pars, res, fields, nssamap, axioms, npre) (pid, Block r1)
        _ -> error "analyser: bstmt is not a BlockStmt"
        
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

-- 
guessInvariant :: (Sort, Args, [AST], Fields, SSAMap) -> Int -> Exp  -> AST -> Z3 AST
guessInvariant (objSort, pars, res, fields, ssamap) pid cond pre = do
    case getCondCounter cond  of 
        Nothing -> error "guessInvariant procedure can't compute valid invariant" -- mkTrue
        Just i -> do
            let (iAST,_,_)  = safeLookup "guessInvariant: i" i ssamap
            -- i >= 0
            i0 <- mkIntNum 0
            c1 <- mkGe iAST i0
            -- exists i. pre
            iApp <- toApp iAST
            ex1 <- mkExistsConst [] [iApp] pre
            -- forall j. i_0 <= j < i => cond
            gen <- generalizeCond (objSort, pars, res, fields, ssamap) i iAST cond pid
            case gen of
                Nothing -> error "guessInvariant procedure can't compute valid invariant" -- mkTrue
                Just genInv -> mkAnd [ex1, genInv, c1]
    
getCondCounter :: Exp -> Maybe Ident
getCondCounter expr = 
    case expr of
        BinOp (ExpName (Name [i])) _ _ -> Just i
        BinOp (BinOp (ExpName (Name [i])) _ _) And _ -> Just i
        BinOp (BinOp (BinOp (ExpName (Name [i])) _ _) _ _) And _ -> Just i
        _ -> Nothing --error $ "getCondCounter: " ++ show expr

generalizeCond :: (Sort, Args, [AST], Fields, SSAMap) -> Ident -> AST -> Exp -> Int -> Z3 (Maybe AST)
generalizeCond env@(objSort, pars, res, fields, ssamap) i iAST _cond pid = do
    case _cond of 
        BinOp _ And cond -> do
            let jIdent = Ident $ "j" ++ show pid
                cond' = replaceExp i jIdent cond
            sort <- mkIntSort
            jSym <- mkStringSymbol $ "j" ++ show pid
            j <- mkConst jSym sort
            jApp <- toApp j
            i0 <- mkIntNum 0
            -- c1: 0 <= j < i
            c1 <- mkLe i0 j >>= \left -> mkLt j iAST >>= \right -> mkAnd [left, right]
            -- c2: 
            let ssamap' = M.insert jIdent (j, sort, pid) ssamap
            c2 <- processExp (objSort, pars, res, fields, ssamap') cond'
            -- \forall j. c1 => c2
            mkImplies c1 c2 >>= \body -> mkForallConst [] [jApp] body >>= \inv -> return $ Just inv
        _ -> return Nothing -- error $ "generalizeCond: " ++ show _cond
    
combine :: (Result, Maybe Model) -> (Result, Maybe Model) -> Z3 (Result, Maybe Model)
combine (Unsat,_) (Unsat,_) = return (Unsat, Nothing)
combine (Unsat,_) res   = return res
combine res   _     = return res

--
helper axioms pre post = do
    assert axioms    
    formula <- mkImplies pre post >>= \phi -> mkNot phi -- >>= \psi -> mkAnd [axioms, psi]
    assert formula
    (r, m) <- getModel
    trace ("helper: " ++ show r) $ return (r,m)

processAssign :: AST -> AssignOp -> AST -> AST -> Z3 AST
processAssign lhs op rhs plhs = do
    case op of 
        EqualA -> mkEq lhs rhs
        AddA -> do
            rhs' <- mkAdd [plhs, rhs]
            mkEq lhs rhs'
        _ -> error $ "processAssign: " ++ show op ++ " not supported"

processType :: Type -> Z3 Sort
processType (PrimType ty) = do 
    case ty of
        BooleanT -> mkBoolSort
        IntT -> mkIntSort
        CharT -> mkIntSort
        DoubleT -> mkIntSort
        FloatT -> mkIntSort
        LongT -> mkIntSort
        _ -> error $ "processType: " ++ show ty ++ " not supported"
processType (RefType (ClassRefType (ClassType [(Ident name,[])]))) = do
    sym <- mkStringSymbol name
    mkUninterpretedSort sym
processType ty@(RefType _) = error $ "processType: not supported " ++ show ty

processNewVar :: (Sort, Args, [AST], Fields, SSAMap, AST) -> Sort -> VarDecl -> Int -> Z3 (SSAMap, AST)
processNewVar (objSort, pars, res, fields, ssamap', pre') sort (VarDecl varid mvarinit) i = do
    (ident, idAst) <- case varid of
        VarId ident@(Ident str) -> do
            let nstr = str ++ show i
            sym <- mkStringSymbol nstr
            var <- mkVar sym sort
            return (ident, var)
        _ -> error $ "processNewVar: not supported " ++ show varid
    let nssamap = M.insert ident (idAst, sort, i) ssamap'
    case mvarinit of
        Nothing -> return (nssamap, pre')
        Just (InitExp expr) -> do
            expAst <- processExp (objSort, pars, res, fields, nssamap) expr
            eqIdExp <- mkEq idAst expAst
            pre <- mkAnd [pre', eqIdExp]
            return (nssamap, pre)
        Just _ -> error "processNewVar: not supported"
    
processExp :: (Sort, Args, [AST], Fields, SSAMap) -> Exp -> Z3 AST
processExp env@(objSort, pars, res, fields, ssamap) expr =
    case expr of
        Lit lit -> processLit lit ssamap
        ExpName name -> processName env name []
        BinOp lhsE op rhsE -> do
            lhs <- processExp env lhsE
            rhs <- processExp env rhsE
            processBinOp op lhs rhs
        FieldAccess fldAccess -> error "processExp: FieldAccess not supported"
        PreMinus nexpr -> do 
            nexprEnc <- processExp env nexpr
            mkUnaryMinus nexprEnc
        MethodInv (MethodCall name args) -> do
            argsAST <- mapM (processExp env) args
            processName env name argsAST
        Cond cond _then _else -> do
            condEnc <- processExp env cond
            _thenEnc <- processExp env _then
            _elseEnc <- processExp env _else
            mkIte condEnc _thenEnc _elseEnc        
        PreNot nexpr -> do
            nexprEnc <- processExp env nexpr
            mkNot nexprEnc            
        _ -> error $  "processExpr: " ++ show expr

processLit :: Literal -> SSAMap -> Z3 AST
processLit (Boolean True) _ = mkTrue
processLit (Boolean False) _ = mkFalse
processLit (Int i) ssamap = mkIntNum i
processLit Null ssamap = mkIntNum 0 -- case M.lookup (Ident "null") ssamap of
--    Nothing -> error "processLit: null not found"
--    Just (ast, _, _) -> return ast
processLit _ _ = error "processLit: not supported"

processName :: (Sort, Args, [AST], Fields, SSAMap) -> Name -> [AST] -> Z3 AST
processName env@(objSort, pars, res, fields, ssamap) (Name [obj]) [] = do
    case M.lookup obj pars of
        Nothing -> case M.lookup obj ssamap of
            Nothing -> error $ "Can't find " ++ show obj
            Just (ast,_,_) -> return ast
        Just ast -> return ast
processName env@(objSort, pars, res, fields, ssamap) (Name [ident]) args = do
    let fn = safeLookup ("processName: declared func")  ident fields
    mkApp fn args
processName env@(objSort, pars, res, fields, ssamap) (Name [Ident "Character",fnName]) args = do
    let fn = safeLookup ("processName: Field" ++ show fnName)  fnName fields
    mkApp fn args
processName env@(objSort, pars, res, fields, ssamap) (Name [Ident "Double",Ident "compare"]) args = do
    let fnName = Ident "compareDouble"
        fn = safeLookup ("processName: Field" ++ show fnName)  fnName fields
    mkApp fn args
processName env@(objSort, pars, res, fields, ssamap) (Name [Ident "Int",Ident "compare"]) args = do
    let fnName = Ident "compareInt"
        fn = safeLookup ("processName: Field" ++ show fnName)  fnName fields
    mkApp fn args
processName env@(objSort, pars, res, fields, ssamap) (Name [Ident "String",Ident "compareIgnoreCase"]) args = do
    let fnName = Ident "compareIgnoreCaseString"
        fn = safeLookup ("processName: Field" ++ show fnName)  fnName fields
    mkApp fn args
processName env@(objSort, pars, res, fields, ssamap) (Name [obj,field]) args = do
    let par = safeLookup ("processName: Object" ++ show obj) obj pars
        fn = safeLookup ("processName: Field" ++ show field)  field fields
    mkApp fn (par:args)
processName env name args = error $  "processName: corner case" ++ show name

processBinOp :: Op -> AST -> AST -> Z3 AST
processBinOp op lhs rhs = do 
    case op of
        NotEq -> mkEq lhs rhs >>= \eq -> mkNot eq
        And -> mkAnd [lhs,rhs]
        Add -> mkAdd [lhs,rhs]
        Sub -> mkSub [lhs,rhs]
        LThan -> mkLt lhs rhs
        LThanE -> mkLe lhs rhs
        GThan -> mkGt lhs rhs
        GThanE -> mkGe lhs rhs
        Equal -> mkEq lhs rhs
        COr -> mkOr [lhs, rhs]
        CAnd -> mkAnd [lhs, rhs]
        _ -> error $ "processBinOp: not supported " ++ show op
    
mkObjectSort :: String -> Z3 Sort
mkObjectSort str = do
    myint <- mkStringSymbol str
    mkUninterpretedSort myint

mkField :: Fields -> VarDecl -> Sort -> Sort -> Z3 Fields
mkField m (VarDecl (VarId (Ident name)) Nothing) parSort retSort = do
--    intSort <- mkIntSort
    fn <- mkFreshFuncDecl name [parSort] retSort
    return $ M.insert (Ident name) fn m

replaceVariable :: String -> FuncDecl -> AST -> Z3 AST
replaceVariable a fnB ast = do
    kind <- getAstKind ast
    case kind of
        Z3_NUMERAL_AST    -> return ast
        Z3_APP_AST        -> do
            app <- toApp ast
            fn <- getAppDecl app
            sym <- getDeclName fn >>= getSymbolString
            if sym == a
            then do
                nArgs <- getAppNumArgs app
                args <- mapM (\i -> getAppArg app i) [0..(nArgs-1)]
                args' <- mapM (replaceVariable a fnB) args
                mkApp fnB args' --T.trace ("FN " ++ symName) $ mkApp fn args'
            else do 
                nArgs <- getAppNumArgs app
                args <- mapM (\i -> getAppArg app i) [0..(nArgs-1)]
                args' <- mapM (replaceVariable a fnB) args
                mkApp fn args' --T.trace ("FN " ++ symName) $ mkApp fn args'
        Z3_VAR_AST        -> return ast
        Z3_QUANTIFIER_AST -> return ast --error "traverse"
        Z3_SORT_AST       -> return ast
        Z3_FUNC_DECL_AST  -> return ast
        Z3_UNKNOWN_AST    -> return ast

    