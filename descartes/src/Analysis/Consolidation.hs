-------------------------------------------------------------------------------
-- Module    :  Analysis.Consolidation
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Consolidation where

import Z3.Monad

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Strict
import Language.Java.Syntax

import Analysis.Types
import Analysis.Util
import Analysis.Props

import Debug.Trace

type ConState = AState Comparator
type SSAMap = Map Ident (AST, Sort, Int)
    
iSSAMap = M.empty

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
    MethodDecl mods ty (Just rty) (Ident name) pars exTy (MethodBody Nothing) -> do
    if name == "get"
    then return m
    else trace ("processing " ++ name ++ " " ++ show (rty,pars) ) $ do
        retSort <- processType rty
        i <- mkIntSort
        parsSort <- mapM processParam pars
        fn <- mkFreshFuncDecl name (objSort:parsSort) retSort
        return $ M.insert (Ident name) fn m

processParam :: FormalParam -> Z3 Sort
processParam (FormalParam mods ty _ _) = processType ty 

verify :: ClassMap -> [Comparator] -> Prop -> Z3 Result
verify classMap comps prop = do
    (objSort, pars, res, fields) <- prelude classMap comps
    (pre, post) <- trace ("after prelude:" ++ show (objSort, pars, res, fields)) $ prop (pars, res, fields)
    let blocks = zip [0..] $ getBlocks comps
    analyser (objSort, pars, res, fields, iSSAMap, pre, post) blocks
    --assert =<< mkNot =<< mkImplies ast post
    --check

-- strongest post condition
analyser :: (Sort, Args, [AST], Fields, SSAMap, AST, AST) -> [(Int, Block)] -> Z3 Result
analyser (objSort, pars, res, fields, ssamap, pre, post) [] = local $ helper pre post
analyser env ((pid,Block []):rest) = analyser env rest
analyser env@(objSort, pars, res, fields, ssamap, pre, post) ((pid,Block (bstmt:r1)):rest) = 
    case bstmt of
        BlockStmt stmt -> case stmt of
            StmtBlock (Block block) -> analyser env ((pid, Block (block ++ r1)):rest)
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
                analyser (objSort, pars, res, fields, ssamap, nPre, post) rest
            IfThenElse cond s1 s2 -> trace ("processing conditional " ++ show cond) $ do
                condSmt <- processExp (objSort, pars, res, fields, ssamap) cond
                -- then branch
                preThen <- trace ("processing THEN branch") $ mkAnd [pre, condSmt]
                push 
                assert preThen
                cThen <- check
                pop 1
                resThen <- case cThen of
                    Unsat -> trace ("preThen becomes false") $ return Unsat
                    _ -> analyser (objSort, pars, res, fields, ssamap, preThen, post) ((pid, Block (BlockStmt s1:r1)):rest)                
                -- else branch
                ncondSmt <- trace ("processing ELSE branch") $ mkNot condSmt
                preElse <- mkAnd [pre, ncondSmt]
                push
                assert preElse
                cElse <- check
                pop 1
                resElse <- case cElse of
                    Unsat -> trace ("preElse becomes false") $ return Unsat
                    _ -> analyser (objSort, pars, res, fields, ssamap, preElse, post) ((pid, Block (BlockStmt s2:r1)):rest)
                trace ("IfThenElse " ++ show (resThen, resElse)) $ combine resThen resElse
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
                        analyser (objSort, pars, res, fields, nssamap, npre, post) ((pid, Block r1):rest)
                    _ -> error $ "Assign " ++ show stmt ++ " not supported"             
            _ -> error $ "not supported: " ++ show stmt
        LocalVars mods ty vars -> do
            sort <- processType ty
            (nssamap, npre) <- foldM (\(ssamap', pre') v -> processNewVar (objSort, pars, res, fields, ssamap', pre') sort v 1) (ssamap, pre) vars
--            let nssamap = foldl (\m (ident,ast, _) -> M.insert ident (ast, sort, 1) m) ssamap idAst
            analyser (objSort, pars, res, fields, nssamap, npre, post) ((pid, Block r1):rest)
        _ -> error "analyser: bstmt is not a BlockStmt"

combine :: Result -> Result -> Z3 Result
combine Unsat Unsat = return Unsat
combine Unsat res   = return res
combine res   _     = return res

--
helper pre post = do
    assert =<< mkNot =<< mkImplies pre post
    check    

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
        _ -> error $ "processType: " ++ show ty ++ " not supported"
processType (RefType _) = error "processType: not supported"

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
        Lit lit -> processLit lit
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
        _ -> error $  "processExpr: " ++ show expr

processLit :: Literal -> Z3 AST
processLit (Int i) = mkIntNum i
processLit _ = error "processLit: not supported"

processName :: (Sort, Args, [AST], Fields, SSAMap) -> Name -> [AST] -> Z3 AST
processName env@(objSort, pars, res, fields, ssamap) (Name [obj]) [] = do
    case M.lookup obj pars of
        Nothing -> case M.lookup obj ssamap of
            Nothing -> error $ "Can't find " ++ show obj
            Just (ast,_,_) -> return ast
        Just ast -> return ast
processName env@(objSort, pars, res, fields, ssamap) (Name [obj,field]) args = do
    let par = safeLookup "processName: Object" obj pars
        fn = safeLookup "processName: Field"  field fields
    mkApp fn (par:args)
processName env _ _ = error "processName: corner case"

processBinOp :: Op -> AST -> AST -> Z3 AST
processBinOp op lhs rhs = do 
    case op of
        Sub -> mkSub [lhs,rhs]
        LThan -> mkLt lhs rhs
        GThan -> mkGt lhs rhs
        GThanE -> mkGe lhs rhs
        Equal -> mkEq lhs rhs
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
    
    
run :: IO ()
run = do 
   evalZ3 forallEx >>= print
   
--test :: Z3     
mytest :: Z3 Result 
mytest = do
    intSort <- mkIntSort
    myIntSort <- mkObjectSort "MyInt"
    valDecl <- mkFreshFuncDecl "val" [myIntSort] intSort
    o1 <- mkFreshConst "o1" myIntSort
    o1val <- mkApp valDecl [o1]
    o2 <- mkFreshConst "o2" intSort
    i0 <- mkIntNum (0 :: Integer)
    sub12 <- mkSub [o1val,o2]
    assert =<< mkNot =<< mkGt sub12 i0
    
    (res, mbModel) <- getModel
    return res
    
test :: Z3 Result
test = do
    intSort <- mkIntSort
    myintSort <- mkObjectSort "MyInt"
    valDecl <- mkFreshFuncDecl "val" [myintSort] intSort
    o1 <- mkFreshConst "o1" myintSort
    o1val <- mkApp valDecl [o1]
    o2 <- mkFreshConst "o2" myintSort
    o2val <- mkApp valDecl [o2]
    o3 <- mkFreshConst "o3" myintSort
    o3val <- mkApp valDecl [o3]
    sub12 <- mkSub [o1val,o2val]
    sub23 <- mkSub [o2val,o3val]
    sub13 <- mkSub [o1val,o3val]
    i0 <- mkIntNum (0 :: Integer)
    r1 <- mkGt sub12 i0
    r2 <- mkGt sub23 i0
    r3 <- mkGt sub13 i0
    r12 <- mkAnd [r1,r2]
    mkImplies r12 r3 >>= mkNot >>= assert
    (res, mbModel) <- getModel
    return res

forallEx :: Z3 Result
forallEx = do
    intSort <- mkIntSort
    compareDecl <- mkFreshFuncDecl "compare" [intSort, intSort] intSort
    x <- mkStringSymbol "x"
    y <- mkStringSymbol "y"
    z <- mkStringSymbol "z"
    xc <- mkConst x intSort
    yc <- mkConst y intSort
    zc <- mkConst z intSort
    i0 <- mkIntNum (0 :: Integer)
    cmpxy <- mkApp compareDecl [xc,yc] >>= \app -> mkGt app i0
    cmpyz <- mkApp compareDecl [yc,zc] >>= \app -> mkGt app i0
    cmpxz <- mkApp compareDecl [xc,zc] >>= \app -> mkGt app i0
    compxyz <- mkAnd [cmpxy, cmpyz]
    ax1bdy <- mkImplies compxyz cmpxz
    axiom1 <- mkForall [] [x,y,z] [intSort,intSort,intSort] ax1bdy
    xPy <- mkAdd [xc,yc]
    yPx <- mkAdd [yc,xc]
    yPz <- mkAdd [yc,zc]
    zPy <- mkAdd [zc,yc]
    cmpXyYx <- mkApp compareDecl [xPy,yPx] >>= \app -> mkGt app i0
    cmpYzZy <- mkApp compareDecl [yPz,zPy] >>= \app -> mkGt app i0
    ax2bdy <- mkImplies cmpxy cmpXyYx
    axiom2 <- mkForall [] [x,y] [intSort,intSort] ax2bdy
    pre <- mkAnd [cmpXyYx,cmpYzZy]
    phi <- mkImplies pre cmpxz >>= mkNot
    mkAnd [axiom1, axiom2] >>= assert
    assert phi
    (res, mbModel) <- getModel
    return res
    
type RetType = ([([Integer], Integer)], Integer)

toIntsPair :: ([AST], AST) -> Z3 ([Integer], Integer)
toIntsPair (is, i) =
    do is' <- mapM getInt is
       i' <- getInt i
       return (is', i')
       
toRetType :: FuncModel -> Z3 RetType
toRetType (FuncModel fs elsePart) =
    do fs' <- mapM toIntsPair fs
       elsePart' <- getInt elsePart
       return (fs', elsePart')
       
funcScript :: Z3 Result
funcScript = do
  -- f :: (Integer,Integer) -> Integer
  intSort <- mkIntSort
  fDecl   <- mkFreshFuncDecl "f" [intSort, intSort] intSort

  -- f(5,10) > 42
  i5      <- mkIntNum (5 :: Integer)
  i10     <- mkIntNum (10 :: Integer)
  i42     <- mkIntNum (42 :: Integer)
  r       <- mkApp fDecl [i5, i10]
  assert =<< mkGt r i42

  -- check satisfiability and obtain model
  (res, mbModel) <- getModel
  return res

    