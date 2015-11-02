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
import qualified Data.Map as M
import Language.Java.Syntax
import Z3.Monad hiding (Params)

--
guessInvariants :: Int -> Exp -> EnvOp [AST]
guessInvariants pid cond = do
 env@Env{..} <- get 
 case getCondCounter cond of 
  Nothing -> error $ "guessInvariant procedure failed at:" ++ show cond -- mkTrue
  Just i -> do
   let (iAST,_,_)  = safeLookup "guessInvariant: i" i _ssamap
   -- i >= 0
   i0 <- lift $ mkIntNum 0
   c1 <- lift $ mkGe iAST i0
   -- exists i. pre
   iApp <- lift $ toApp iAST
   ex1 <- lift $ mkExistsConst [] [iApp] _pre
   -- forall j. i_0 <= j < i => cond
   gen <- lift $ generalizeCond (_objSort,_params,_res,_fields,_ssamap) i iAST cond pid
   case gen of
    Nothing -> error "guessInvariant procedure can't compute valid invariant" -- mkTrue
    Just genInv -> do
      inv <- lift $ mkAnd [ex1, genInv, c1]
      return [inv]
    
getCondCounter :: Exp -> Maybe Ident
getCondCounter expr = 
  case expr of
    BinOp (ExpName (Name [i])) _ _ -> Just i
    BinOp (BinOp (ExpName (Name [i])) _ _) And _ -> Just i
    BinOp (BinOp (BinOp (ExpName (Name [i])) _ _) _ _) And _ -> Just i
    _ -> Nothing --error $ "getCondCounter: " ++ show expr

generalizeCond :: (Sort, Params, [AST], Fields, SSAMap) -> Ident -> AST -> Exp -> Int -> Z3 (Maybe AST)
generalizeCond env@(objSort, pars, res, fields, ssamap) i iAST _cond pid =
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
    _ -> do
      let jIdent = Ident $ "j" ++ show pid
      sort <- mkIntSort
      jSym <- mkStringSymbol $ "j" ++ show pid
      j <- mkConst jSym sort
      jApp <- toApp j
      i0 <- mkIntNum 0
      -- c1: 0 <= j < i
      c1 <- mkLe i0 j >>= \left -> mkLt j iAST >>= \right -> mkAnd [left, right]
      -- c2: 
      let ssamap' = M.insert jIdent (j, sort, pid) ssamap
      c2 <- buildArtCond (pars,fields) j pid
      -- \forall j. c1 => c2
      mkImplies c1 c2 >>= \body -> mkForallConst [] [jApp] body >>= \inv -> return $ Just inv 
    
buildArtCond :: (Params, Fields) -> AST -> Int -> Z3 AST
buildArtCond (pars,fields) j pid = do
  let o1 = Ident $ "o1" ++ show pid
      o2 = Ident $ "o2" ++ show pid
      getfn = Ident "get"
      o1z3 = safeLookup "buildArtCond: o1" o1 pars
      o2z3 = safeLookup "buildArtCond: o2" o2 pars
      fn = safeLookup "buildArtCond: fn" getfn fields
  get1 <- mkApp fn [o1z3,j]
  get2 <- mkApp fn [o2z3,j]
  mkEq get1 get2