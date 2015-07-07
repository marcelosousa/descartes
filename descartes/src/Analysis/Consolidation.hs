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
import Analysis.Props

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

verify :: ClassMap -> [Comparator] -> Prop -> Z3 (Result, Maybe String)
verify classMap comps prop = do
    (objSort, pars, res, fields) <- prelude classMap comps
    (pre, post) <- trace ("after prelude:" ++ show (objSort, pars, res, fields)) $ prop (pars, res, fields)
    (fields', axioms) <- addAxioms objSort fields
    let blocks = zip [0..] $ getBlocks comps
    iSSAMap <- getInitialSSAMap
    (res, mmodel) <- analyser (objSort, pars, res, fields', iSSAMap, axioms, pre, pre, post) blocks
    case res of 
        Unsat -> return (Unsat, Nothing)
        Sat -> do
            str <- showModel $ fromJust mmodel
            return (Sat, Just str)

addAxioms :: Sort -> Fields -> Z3 (Fields, AST)
addAxioms objSort fields = do
    iSort <- mkIntSort
    fnDouble <- mkFreshFuncDecl "compareDouble" [iSort, iSort] iSort
    fnInt <- mkFreshFuncDecl "compareInt" [iSort, iSort] iSort
    fnStr <- mkFreshFuncDecl "compareIgnoreCaseString" [objSort, objSort] iSort
    let fields' = M.insert (Ident "compareDouble") fnDouble $ M.insert (Ident "compareInt") fnInt $ M.insert (Ident "compareIgnoreCaseString") fnStr fields
    -- add anti-symmetry axiom for Double.compare
    asymAxiomDouble <- genAntiSymAxiom iSort fnDouble
    -- add anti-symmetry axiom for Double.compare
    asymAxiomInt <- genAntiSymAxiom iSort fnInt    
    -- add anti-symmetry axiom for String.compareIgnoreCase
    asymAxiomStr <- genAntiSymAxiom objSort fnStr
    axioms <- mkAnd [asymAxiomInt, asymAxiomDouble, asymAxiomStr]
    return (fields', axioms)

-- Generate anti-symmetry axiom
genAntiSymAxiom :: Sort -> FuncDecl -> Z3 AST
genAntiSymAxiom sort fn = do    
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym sort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym sort
    yApp <- toApp y
    i0 <- mkIntNum 0
    lhsbody <- mkApp fn [x,y] >>= \a -> mkGt a i0
    rhsbody <- mkApp fn [y,x] >>= \a -> mkLt a i0
    body <- mkIff lhsbody rhsbody
    mkForallConst [] [xApp, yApp] body


-- strongest post condition
analyser :: (Sort, Args, [AST], Fields, SSAMap, AST, AST, AST, AST) -> [(Int, Block)] -> Z3 (Result, Maybe Model)
analyser (objSort, pars, res, fields, ssamap, axioms, iPre, pre, post) [] = do 
    --preStr  <- astToString pre
    --postStr <- astToString post
    --let k = trace ("Analyser Leaf:\nPrecondition:\n" ++ preStr ++ "\nPostcondition: " ++ postStr) $ unsafePerformIO $ getChar
    local $ helper axioms pre post
analyser env ((pid,Block []):rest) = analyser env rest
analyser env@(objSort, pars, res, fields, ssamap, axioms, iPre, pre, post) ((pid,Block (bstmt:r1)):rest) = do
    --preStr  <- astToString pre
    --postStr <- astToString post
    --let k = trace ("Analyser State:\nStatement: " ++ prettyPrint bstmt ++ "\nPrecondition:\n" ++ preStr ++ "\nPostcondition: " ++ postStr) $ unsafePerformIO $ getChar
    case bstmt of
        BlockStmt stmt -> case stmt of
            StmtBlock (Block block) -> analyser env ((pid, Block (block ++ r1)):rest)
            Assume expr -> do
                exprEnc <- processExp (objSort, pars, res, fields, ssamap) expr
                nPre <- mkAnd [pre,exprEnc]
                analyser (objSort, pars, res, fields, ssamap, axioms, iPre, nPre, post) ((pid, Block r1):rest)
            Return Nothing -> error "analyser: return Nothing"
            Return (Just expr) -> trace ("processing return of pid " ++ show pid ++ " " ++ show stmt) $ do
                exprPsi <- processExp (objSort, pars, res, fields, ssamap) expr
                let resPid = res !! pid  
                r <- mkEq resPid exprPsi
                nPre <- mkAnd [pre,r]
             --   test <- local $ helper nPre post
             --   trace ("return test = " ++ show test) $ case test of
             --       Unsat -> analyser (objSort, pars, res, fields, nPre, post) rest -- trace ("stopped") $ return test
             --       _ -> analyser (objSort, pars, res, fields, nPre, post) rest
                analyser (objSort, pars, res, fields, ssamap, axioms, iPre, nPre, post) rest
            IfThen cond s1 -> do
                condSmt <- processExp (objSort, pars, res, fields, ssamap) cond
                -- then branch
                preThen <- trace ("processing THEN branch") $ mkAnd [pre, condSmt]
                push 
                assert preThen
                cThen <- check
                pop 1
                resThen <- case cThen of
                    Unsat -> trace ("preThen becomes false") $ return (Unsat,Nothing)
                    _ -> analyser (objSort, pars, res, fields, ssamap, axioms, iPre, preThen, post) ((pid, Block (BlockStmt s1:r1)):rest)                
                -- else branch
                ncondSmt <- trace ("processing ELSE branch") $ mkNot condSmt
                preElse <- mkAnd [pre, ncondSmt]
                push
                assert preElse
                cElse <- check
                pop 1
                resElse <- case cElse of
                    Unsat -> trace ("preElse becomes false") $ return (Unsat,Nothing)
                    _ -> analyser (objSort, pars, res, fields, ssamap, axioms, iPre, preElse, post) ((pid, Block r1):rest)
                trace ("IfThen " ++ show (fst resThen, fst resElse)) $ combine resThen resElse
            IfThenElse Nondet s1 s2 -> do 
                resThen <- analyser (objSort, pars, res, fields, ssamap, axioms, iPre, pre, post) ((pid, Block (BlockStmt s1:r1)):rest)                
                resElse <- analyser (objSort, pars, res, fields, ssamap, axioms, iPre, pre, post) ((pid, Block (BlockStmt s2:r1)):rest)
                trace ("NonDet IfThenElse " ++ show (fst resThen, fst resElse)) $ combine resThen resElse                
            IfThenElse cond s1 s2 -> trace ("processing conditional " ++ show cond) $ do
                condSmt <- processExp (objSort, pars, res, fields, ssamap) cond
                -- then branch
                preThen <- trace ("processing THEN branch") $ mkAnd [pre, condSmt]
                push 
                assert preThen
                cThen <- check
                pop 1
                resThen <- case cThen of
                    Unsat -> trace ("preThen becomes false") $ return (Unsat,Nothing)
                    _ -> analyser (objSort, pars, res, fields, ssamap, axioms, iPre, preThen, post) ((pid, Block (BlockStmt s1:r1)):rest)                
                -- else branch
                ncondSmt <- trace ("processing ELSE branch") $ mkNot condSmt
                preElse <- mkAnd [pre, ncondSmt]
                push
                assert preElse
                cElse <- check
                pop 1
                resElse <- case cElse of
                    Unsat -> trace ("preElse becomes false") $ return (Unsat,Nothing)
                    _ -> analyser (objSort, pars, res, fields, ssamap, axioms, iPre, preElse, post) ((pid, Block (BlockStmt s2:r1)):rest)
                trace ("IfThenElse " ++ show (fst resThen, fst resElse)) $ combine resThen resElse
            ExpStmt (Assign lhs aOp rhs) -> do
                rhsAst <- processExp (objSort, pars, res, fields, ssamap) rhs
                case lhs of
                    NameLhs (Name [ident@(Ident str)]) -> do
                        let (plhsAST,sort, i) = safeLookup "Assign" ident ssamap
                            ni = i+1
                            nstr = str ++ show ni
                        sym <- mkStringSymbol nstr
                        var <- mkVar sym sort
                        let nssamap = M.insert ident (var, sort, ni) ssamap
                        ass <- processAssign var aOp rhsAst plhsAST
                        npre <- mkAnd [pre, ass]
                        analyser (objSort, pars, res, fields, nssamap, axioms, iPre, npre, post) ((pid, Block r1):rest)
                    _ -> error $ "Assign " ++ show stmt ++ " not supported"
            ExpStmt (PostIncrement lhs) -> do
                rhsAst <- processExp (objSort, pars, res, fields, ssamap) (BinOp lhs Add (Lit $ Int 1))
                case lhs of
                    ExpName (Name [ident@(Ident str)]) -> do
                        let (plhsAST,sort, i) = safeLookup "Assign" ident ssamap
                            ni = i+1
                            nstr = str ++ show ni
                        sym <- mkStringSymbol nstr
                        var <- mkVar sym sort
                        let nssamap = M.insert ident (var, sort, ni) ssamap
                        ass <- processAssign var EqualA rhsAst plhsAST
                        npre <- mkAnd [pre, ass]
                        analyser (objSort, pars, res, fields, nssamap, axioms, iPre, npre, post) ((pid, Block r1):rest)
                    _ -> error $ "PostIncrement " ++ show stmt ++ " not supported"
            ExpStmt (PostDecrement lhs) -> do
                rhsAst <- processExp (objSort, pars, res, fields, ssamap) (BinOp lhs Sub (Lit $ Int 1))
                case lhs of
                    ExpName (Name [ident@(Ident str)]) -> do
                        let (plhsAST,sort, i) = safeLookup "Assign" ident ssamap
                            ni = i+1
                            nstr = str ++ show ni
                        sym <- mkStringSymbol nstr
                        var <- mkVar sym sort
                        let nssamap = M.insert ident (var, sort, ni) ssamap
                        ass <- processAssign var EqualA rhsAst plhsAST
                        npre <- mkAnd [pre, ass]
                        analyser (objSort, pars, res, fields, nssamap, axioms, iPre, npre, post) ((pid, Block r1):rest)
                    _ -> error $ "PostIncrement " ++ show stmt ++ " not supported"
            While _cond _body -> trace ("\nProcessing While loop from PID" ++ show pid ++"\n") $ do
                inv <- mkTrue -- buildInvariant pars ssamap fields (pid+1)
                checkInv <- local $ helper axioms pre inv
                invStr <- astToString inv
                preStr <- astToString pre
                trace ("\nPrecondition:\n"++ preStr ++ "\nInvariant:\n" ++ invStr) $ case checkInv of
                    (Unsat,_) -> do
                        condAst <- processExp (objSort, pars, res, fields, ssamap) _cond
                        ncondAst <- mkNot condAst
                        checkInv' <- mkAnd [inv, ncondAst] >>= \npre -> local $ helper axioms npre inv
                        case checkInv' of
                            (Unsat,_) -> do
                                nPre <- mkAnd [inv, condAst]
                                let s = [(pid, Block [BlockStmt _body])] 
                                bodyCheck <- local $ analyser (objSort, pars, res, fields, ssamap, axioms, iPre, nPre, inv) s
                                case bodyCheck of
                                    (Unsat,_) -> do 
                                        rPre <- mkAnd [inv, pre]
                                        analyser (objSort, pars, res, fields, ssamap, axioms, iPre, pre, post) ((pid, Block r1):rest)
                                    _ -> error $ "bodyCheck: SAT"
                    _ -> error "precondition does not imply the invariant"
            _ -> error $ "not supported: " ++ show stmt
        LocalVars mods ty vars -> do
            sort <- processType ty
            (nssamap, npre) <- foldM (\(ssamap', pre') v -> processNewVar (objSort, pars, res, fields, ssamap', pre') sort v 1) (ssamap, pre) vars
--            let nssamap = foldl (\m (ident,ast, _) -> M.insert ident (ast, sort, 1) m) ssamap idAst
            analyser (objSort, pars, res, fields, nssamap, axioms, iPre, npre, post) ((pid, Block r1):rest)
        _ -> error "analyser: bstmt is not a BlockStmt"

buildInvariant :: Args -> SSAMap -> Fields -> Int -> Z3 AST
buildInvariant args ssamap fields pid = do
    let i = Ident $ "i" ++ show pid
        (iAST,iSort,_)  = safeLookup "buildInvariant: i" i ssamap
        thisPos = Ident $ "thisPos" ++ show pid
        (thisPosAST,_,_) = safeLookup "buildInvariant: thisPos" thisPos ssamap
        otherPos = Ident $ "otherPos" ++ show pid
        (otherPosAST,_,_) = safeLookup "buildInvariant: otherPos" otherPos ssamap
        getFn = safeLookup ("buildInvariant: get" ++ show fields) (Ident "get") fields
        o1 = safeLookup "buildInvariant: o1x" (Ident $ "o1" ++ show pid) args
        o2 = safeLookup "buildInvariant: o2x" (Ident $ "o2" ++ show pid) args
    j <- mkStringSymbol $ "j" ++ show pid
    jc <- mkConst j iSort
    lhs <- mkAdd [thisPosAST, jc] >>= \s -> mkApp getFn [o1,s]
    rhs <- mkAdd [otherPosAST, jc] >>= \s -> mkApp getFn [o2,s]    
    rhsbody <- mkEq lhs rhs 
    i0 <- mkIntNum 0
    lhsbody <- mkLe i0 jc >>= \s1 -> mkLt jc iAST >>= \s2 -> mkAnd [s1, s2]
    body <- mkImplies lhsbody rhsbody
    mkForall [] [j] [iSort] body
    
    
combine :: (Result, Maybe Model) -> (Result, Maybe Model) -> Z3 (Result, Maybe Model)
combine (Unsat,_) (Unsat,_) = return (Unsat, Nothing)
combine (Unsat,_) res   = return res
combine res   _     = return res

--
helper axioms pre post = do
    assert axioms    
    formula <- mkImplies pre post >>= \phi -> mkNot phi -- >>= \psi -> mkAnd [axioms, psi]
    assert formula
    getModel

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
    

    