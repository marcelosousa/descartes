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

import Debug.Trace

type ConState = AState Comparator

-- receives the parameters and returns to specify the pre and post-condition
-- need to use maps for the parameters, returns, fields
type Args = Map Ident AST
type Res  = [AST]
type Fields = Map Ident FuncDecl

type Prop = (Args, Res) -> Z3 (AST, AST)

testProp :: Prop
testProp (args, [res1]) = do
    i0 <- mkIntNum (0 :: Integer)
    post <- mkGt res1 i0
    pre <- mkTrue
    return (pre, post)
    
transitivity :: Prop
transitivity (args, [res1,res2,res3]) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o13 = safeLookup "trans" (Ident "o13") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
        o23 = safeLookup "trans" (Ident "o23") args    
    eq1 <- mkEq o11 o13
    eq2 <- mkEq o21 o12
    eq3 <- mkEq o22 o23
    pre <- mkAnd [eq1,eq2,eq3]
    i0 <- mkIntNum (0 :: Integer)
    r1 <- mkGt res1 i0
    r2 <- mkGt res2 i0
    r3 <- mkGt res3 i0
    r12 <- mkAnd [r1,r2]
    pos <- mkImplies r12 r3
    return (pre, pos)

antisymmetry :: Prop
antisymmetry (args, [res1, res2]) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
    pre <- mkAnd [eq1,eq2]
    i0 <- mkIntNum (0 :: Integer)
    r1 <- mkGt res1 i0
    r2 <- mkLt res2 i0
    r12 <- mkImplies r1 r2
    r21 <- mkImplies r2 r1
    pos <- mkAnd [r12, r21]
    return (pre,pos)
    
-- result: (ObjectType, Parameters, Results)
prelude :: ClassMap -> [Comparator] -> Z3 (Sort, Args, [AST], Fields)
prelude classMap comps = do
    let arity = length comps
    if arity == 0
    then error "init: empty comparators"
    else do
        let objType = getObjectType $ head comps
        objSort <- mkObjectSort objType
        let fieldNames = getFieldNames objType classMap
        fields <- mapM (\f -> mkField f objSort) fieldNames
        let fields' = foldl (\r (k,v) -> M.insert (Ident k) v r) M.empty $ zip fieldNames fields
            parsId = concatMap (getParIdents . getParameters) comps
        pars <- mapM (\par -> mkFreshConst par objSort) parsId
        let pars' = foldl (\r (k,v) -> M.insert (Ident k) v r) M.empty $ zip parsId pars
        intSort <- mkIntSort
        res <- mapM (\idx -> mkFreshConst ("res"++show idx) intSort) [1..arity]
        return (objSort, pars', res, fields')

verify :: ClassMap -> [Comparator] -> Prop -> Z3 Result
verify classMap comps prop = do
    (objSort, pars, res, fields) <- prelude classMap comps
    (pre, post) <- trace ("after prelude:" ++ show (objSort, pars, res, fields)) $ prop (pars, res)
    let blocks = getBlocks comps
    ast <- analyse (objSort, pars, res, fields, pre) blocks
    assert =<< mkNot =<< mkImplies ast post
    check
    
analyse :: (Sort, Args, [AST], Fields, AST) -> [Block] -> Z3 AST
analyse (objSort, pars, res, fields, psi) progs = 
    foldM (\fml (prog, pid) -> process (objSort, pars, res, fields, fml) pid prog) psi $ zip progs [0..]

process :: (Sort, Args, [AST], Fields, AST) -> Int -> Block -> Z3 AST
process (objSort, pars, res, fields, psi) pid (Block bstmt) = -- needs to change
    foldM (\fml stmt -> processBlock (objSort, pars, res, fields, fml) pid stmt) psi bstmt

processBlock :: (Sort, Args, [AST], Fields, AST) -> Int -> BlockStmt -> Z3 AST
processBlock (objSort, pars, res, fields, psi) pid bstmt =
    case bstmt of
        BlockStmt stmt -> processStmt (objSort, pars, res, fields, psi) pid stmt
        _ -> error "processBlock"

processStmt :: (Sort, Args, [AST], Fields, AST) -> Int -> Stmt -> Z3 AST
processStmt (objSort, pars, res, fields, psi) pid stmt = 
    case stmt of
        StmtBlock block -> process (objSort, pars, res, fields, psi) pid block
        Return Nothing -> error "processStmt: return nothing"
        Return (Just expr) -> trace ("processing return of pid " ++ show psi) $ do
            exprPsi <- processExp (objSort, pars, res, fields) expr
            let resPid = res !! pid  
            r <- trace ("making equality between" ++ show resPid) $ mkEq resPid exprPsi
            mkAnd [psi,r]
        IfThen cond stm -> do
            cpsi <- processExp (objSort, pars, res, fields) cond
            psi' <- mkAnd [cpsi,psi]
            return psi'

processExp :: (Sort, Args, [AST], Fields) -> Exp -> Z3 AST
processExp env@(objSort, pars, res, fields) expr =
    case expr of
        Lit lit -> processLit lit
        ExpName name -> processName env name
        BinOp lhsE op rhsE -> do
            lhs <- processExp env lhsE
            rhs <- processExp env rhsE
            processBinOp op lhs rhs
        FieldAccess fldAccess -> error "processExp: FieldAccess not supported"
        _ -> undefined
        
processLit :: Literal -> Z3 AST
processLit (Int i) = mkIntNum i
processLit _ = error "processLit: not supported"

processName :: (Sort, Args, [AST], Fields) -> Name -> Z3 AST
processName env@(objSort, pars, res, fields) (Name [obj,field]) = do
    let par = safeLookup "processName: Object" obj pars
        fn = safeLookup "processName: Field"  field fields
    mkApp fn [par]

processBinOp :: Op -> AST -> AST -> Z3 AST
processBinOp op lhs rhs = do 
    case op of
        Sub -> mkSub [lhs,rhs]
        _ -> error "processBinOp: not supported"
    
mkObjectSort :: String -> Z3 Sort
mkObjectSort str = do
    myint <- mkStringSymbol str
    mkUninterpretedSort myint

mkField :: String -> Sort -> Z3 FuncDecl
mkField name sort = do
    intSort <- mkIntSort
    mkFreshFuncDecl name [sort] intSort
    
run :: IO ()
run = do 
   evalZ3 funcScript >>= print
   evalZ3 test >>= print
   
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

    