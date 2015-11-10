-------------------------------------------------------------------------------
-- Module    :  Analysis.Engine
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Engine where

import Analysis.Axioms
import Analysis.Properties
import Analysis.Types
import Analysis.Util

import Control.Monad.State.Strict
import Data.Map (Map)
import Data.Maybe

import Language.Java.Syntax
import Language.Java.Pretty

import System.IO.Unsafe
import Z3.Monad hiding (Params)

import qualified Data.Map as M
import qualified Debug.Trace as T

trace a b = b
--trace = T.trace

-- Performs a SAT-query.
checkSAT phi = do
  push 
  assert phi
  res <- check
  pop 1
  return res
  
helper axioms pre post = do
  assert axioms    
  formula <- mkImplies pre post >>= \phi -> mkNot phi -- >>= \psi -> mkAnd [axioms, psi]
  assert formula
  (r, m) <- getModel
  trace ("helper: " ++ show r) $ return (r,m)

getInitialSSAMap :: Z3 SSAMap
getInitialSSAMap = do
  iSort <- mkIntSort
  fn <- mkFreshFuncDecl "null" [] iSort
  ast <- mkApp fn []
  return $ M.singleton (Ident "null") (ast, iSort, 0)

-- result: (ObjectType, Parameters, Results)
prelude :: ClassMap -> [Comparator] -> Z3 (Sort, Params, [AST], Fields)
prelude classMap comps = do
  let arity = length comps
  if arity == 0
  then error "init: empty comparators"
  else do
    let objType = getObjectType $ head comps
    objSort <- mkObjectSort objType
    let objFields = fields $ safeLookup "prelude" objType classMap
        fieldNames = getFieldNames objType classMap
--    fields <- mapM (\f -> mkField f objSort) fieldNames
    let --fields' = foldl (\r (k,v) -> M.insert (Ident k) v r) M.empty $ zip fieldNames fields
        parsId = concatMap (getParIdents . getParameters) comps
    -- methods
    fields' <- foldM (mkAttribute objSort) M.empty objFields
    pars <- mapM (\par -> mkFreshConst par objSort) parsId
    let pars' = foldl (\r (k,v) -> M.insert (Ident k) v r) M.empty $ zip parsId pars
    intSort <- mkIntSort
    res <- mapM (\idx -> mkFreshConst ("res"++show idx) intSort) [1..arity]
    return (objSort, pars', res, fields')

-- SMT Utility Functions
mkAttribute :: Sort -> Fields -> MemberDecl -> Z3 Fields
mkAttribute objSort m mDecl =
  case mDecl of
    FieldDecl  mods ty vardecls -> do
      retSort <- processType ty
      foldM (\nm vardecl -> mkField nm vardecl objSort retSort) m vardecls 
    MethodDecl mods ty (Just rty) (Ident name) pars exTy (MethodBody Nothing) -> trace ("processing " ++ name ++ " " ++ show (rty,pars) ) $ do
      retSort <- processType rty
      i <- mkIntSort
      parsSort <- mapM processParam pars
      fn <- mkFreshFuncDecl name (objSort:parsSort) retSort
      return $ M.insert (Ident name) fn m

mkObjectSort :: String -> Z3 Sort
mkObjectSort str = do
  myint <- mkStringSymbol str
  mkUninterpretedSort myint

mkField :: Fields -> VarDecl -> Sort -> Sort -> Z3 Fields
mkField m (VarDecl (VarId (Ident name)) Nothing) parSort retSort = do
--  intSort <- mkIntSort
  fn <- mkFreshFuncDecl name [parSort] retSort
  return $ M.insert (Ident name) fn m

-- Processing Functions
processParam :: FormalParam -> Z3 Sort
processParam (FormalParam mods ty _ _) = processType ty 

processType :: Type -> Z3 Sort
processType (PrimType ty) =
  case ty of
    BooleanT -> mkBoolSort
    _ -> mkIntSort -- error $ "processType: " ++ show ty ++ " not supported"
processType (RefType (ClassRefType (ClassType [(Ident name,[])]))) = mkIntSort
--  do
--    sym <- mkStringSymbol name
--    mkUninterpretedSort sym
processType ty@(RefType _) = error $ "processType: not supported " ++ show ty

processAssign :: AST -> AssignOp -> AST -> AST -> Z3 AST
processAssign lhs op rhs plhs =
  case op of 
    EqualA -> mkEq lhs rhs
    AddA -> do
      rhs' <- mkAdd [plhs, rhs]
      mkEq lhs rhs'
    _ -> error $ "processAssign: " ++ show op ++ " not supported"

processNewVar :: (Sort, Params, [AST], Fields, SSAMap, AssignMap, AST) -> Sort -> VarDecl -> Int -> Z3 (SSAMap, AssignMap, AST)
processNewVar (objSort, pars, res, fields, ssamap', _assmap, pre') sort (VarDecl varid mvarinit) i = do
  (ident, idAst) <-
    case varid of
      VarId ident@(Ident str) -> do
        let nstr = str ++ show i
        sym <- mkStringSymbol nstr
        var <- mkVar sym sort
        return (ident, var)
      _ -> error $ "processNewVar: not supported " ++ show varid
  let nssamap = M.insert ident (idAst, sort, i) ssamap'
  case mvarinit of
    Nothing -> return (nssamap, _assmap, pre')
    Just (InitExp expr) -> do
      expAst <- processExp (objSort, pars, res, fields, nssamap) expr
      eqIdExp <- mkEq idAst expAst
      pre <- mkAnd [pre', eqIdExp]
      let assmap = M.insert ident expr _assmap
      return (nssamap, assmap, pre)
    Just _ -> error "processNewVar: not supported"
    
processExp :: (Sort, Params, [AST], Fields, SSAMap) -> Exp -> Z3 AST
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
    PreNot nexpr -> do
      nexprEnc <- processExp env nexpr
      mkNot nexprEnc            
    _ -> error $  "processExpr: " ++ show expr

processLit :: Literal -> Z3 AST
processLit lit =
  case lit of
    Boolean True -> mkTrue
    Boolean False -> mkFalse
    Int i -> mkIntNum i
    Null -> mkIntNum 0 -- case M.lookup (Ident "null") ssamap of
--    Nothing -> error "processLit: null not found"
--    Just (ast, _, _) -> return ast
    _ -> error "processLit: not supported"

processName :: (Sort, Params, [AST], Fields, SSAMap) -> Name -> [AST] -> Z3 AST
processName env@(objSort, pars, res, fields, ssamap) (Name [obj]) [] =
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
    Mult -> mkMul [lhs,rhs]
    Sub -> mkSub [lhs,rhs]
    LThan -> mkLt lhs rhs
    LThanE -> mkLe lhs rhs
    GThan -> mkGt lhs rhs
    GThanE -> mkGe lhs rhs
    Equal -> mkEq lhs rhs
    COr -> mkOr [lhs, rhs]
    CAnd -> mkAnd [lhs, rhs]
    _ -> error $ "processBinOp: not supported " ++ show op
    
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
        nParams <- getAppNumArgs app
        args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
        args' <- mapM (replaceVariable a fnB) args
        mkApp fnB args' --T.trace ("FN " ++ symName) $ mkApp fn args'
      else do 
        nParams <- getAppNumArgs app
        args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
        args' <- mapM (replaceVariable a fnB) args
        mkApp fn args' --T.trace ("FN " ++ symName) $ mkApp fn args'
    Z3_VAR_AST        -> return ast
    Z3_QUANTIFIER_AST -> return ast --error "traverse"
    Z3_SORT_AST       -> return ast
    Z3_FUNC_DECL_AST  -> return ast
    Z3_UNKNOWN_AST    -> return ast

