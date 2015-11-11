{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Invariant
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Invariant where

import Analysis.Engine
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map as M
import Language.Java.Syntax
import Z3.Monad hiding (Params)

import qualified Debug.Trace as T

guessInvariants :: Int -> Exp -> Stmt -> EnvOp [AST]
guessInvariants pid cond body = do
  env@Env{..} <- get
  let fnCalls = getFnCalls cond ++ getFnCalls body
  fnNames' <- lift $ foldM (search _fields) [] fnCalls 
  let fnNames = nub fnNames'
  increasing <- guessInvariant fnNames mkGe mkLe mkLt pid cond
  decreasing <- guessInvariant fnNames mkLe mkGe mkGt pid cond
  return $ increasing ++ decreasing

search fields list (Name n) =
  case n of 
    [ident] ->
      case M.lookup ident fields of
        Nothing -> return list
        Just fn -> return $ fn:list
    [Ident "Character",fnName] ->
      case M.lookup fnName fields of
        Nothing -> return list
        Just fn -> return $ fn:list
    [Ident "Double",Ident "compare"] -> 
      let fnName = Ident "compareDouble"
      in case M.lookup fnName fields of
        Nothing -> return list
        Just fn -> return $ fn:list
    [Ident "Int",Ident "compare"] -> 
      let fnName = Ident "compareInt"
      in case M.lookup fnName fields of
        Nothing -> return list
        Just fn -> return $ fn:list
    [Ident "String",Ident "compareIgnoreCase"] -> 
      let fnName = Ident "compareIgnoreCaseString"
      in case M.lookup fnName fields of
        Nothing -> return list
        Just fn -> return $ fn:list
    [obj,field] ->
      case M.lookup field fields of
        Nothing -> return list
        Just fn -> return $ fn:list
    _ -> return list
        
type Z3Op = (AST -> AST -> Z3 AST)
--
guessInvariant :: [FuncDecl] -> Z3Op -> Z3Op -> Z3Op -> Int -> Exp -> EnvOp [AST]
guessInvariant fnNames op op' op'' pid cond = do
 env@Env{..} <- get 
 case getCondCounter cond of 
  Nothing -> error "none of the invariants was able to prove the property." -- at:" ++ show cond -- mkTrue
  Just i -> do
   let (iAST,_,_)  = safeLookup "guessInvariant: i" i _ssamap
       e = safeLookup ("getting last condition assignment" ++ show i) i _assmap
   -- i `op` Init
   i0 <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) e --T.trace (show e) $ lift $ mkIntNum 0
   c1 <- lift $ op iAST i0
   -- exists i. pre
   iApp <- lift $ toApp iAST
   ex1 <- lift $ mkExistsConst [] [iApp] _pre
   -- forall j. i_0 <= j < i => cond
   gen <- lift $ generalizeCond fnNames op' op'' (_objSort,_params,_res,_fields,_ssamap) i0 i iAST cond pid
   lift $ mapM (\genInv -> mkAnd [ex1, genInv, c1]) gen
    
getCondCounter :: Exp -> Maybe Ident
getCondCounter expr = 
  case expr of
    BinOp (ExpName (Name [i])) _ _ -> Just i
    BinOp (BinOp (ExpName (Name [i])) _ _) And _ -> Just i
    BinOp (BinOp (BinOp (ExpName (Name [i])) _ _) _ _) And _ -> Just i
    _ -> Nothing --error $ "getCondCounter: " ++ show expr

generalizeCond :: [FuncDecl] -> Z3Op -> Z3Op -> (Sort, Params, [AST], Fields, SSAMap) ->  AST -> Ident -> AST -> Exp -> Int -> Z3 [AST]
generalizeCond fnNames op op' env@(objSort, pars, res, fields, ssamap) i0 i iAST _cond pid =
  case _cond of 
    BinOp _ And cond -> do
      let jIdent = Ident $ "j" ++ show pid
          cond' = replaceExp i jIdent cond
      sort <- mkIntSort
      jSym <- mkStringSymbol $ "j" ++ show pid
      j <- mkConst jSym sort
      jApp <- toApp j
--      let (i0,_,_) = safeLookup "genCond" (Ident $ "myPosition"++show pid) ssamap
--      i0 <- mkIntNum 0
      -- c1: Init <= j < i or i < j <= Init
      c1 <- op i0 j >>= \left -> op' j iAST >>= \right -> mkAnd [left, right]
      -- c2: 
      let ssamap' = M.insert jIdent (j, sort, pid) ssamap
      c2 <- processExp (objSort, pars, res, fields, ssamap') cond'
      -- \forall j. c1 => c2
      mkImplies c1 c2 >>= \body -> mkForallConst [] [jApp] body >>= \inv -> return [inv]
    _ -> do
      let jIdent = Ident $ "j" ++ show pid
      sort <- mkIntSort
      jSym <- mkStringSymbol $ "j" ++ show pid
      j <- mkConst jSym sort
      jApp <- toApp j
--      i0 <- mkIntNum 0
      -- c1: 0 <= j < i
      c1 <- op i0 j >>= \left -> op' j iAST >>= \right -> mkAnd [left, right]
      -- c2: 
      let ssamap' = M.insert jIdent (j, sort, pid) ssamap
      c2s <- mapM (\fnName -> buildArtCond fnName (pars,fields) j pid) fnNames
      -- \forall j. c1 => c2
      mapM (\c2 -> mkImplies c1 c2 >>= \body -> mkForallConst [] [jApp] body) c2s
    
buildArtCond :: FuncDecl -> (Params, Fields) -> AST -> Int -> Z3 AST
buildArtCond fn (pars,fields) j pid = do
  let o1 = Ident $ "o1" ++ show pid
      o2 = Ident $ "o2" ++ show pid
      o1z3 = safeLookup "buildArtCond: o1" o1 pars
      o2z3 = safeLookup "buildArtCond: o2" o2 pars
  get1 <- mkApp fn [o1z3,j]
  get2 <- mkApp fn [o2z3,j]
  mkEq get1 get2
