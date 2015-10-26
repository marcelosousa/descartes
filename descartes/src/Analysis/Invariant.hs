-------------------------------------------------------------------------------
-- Module    :  Analysis.Invariant
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Invariant where

import Analysis.Engine
import Analysis.Properties
import Analysis.Util
import qualified Data.Map as M
import Language.Java.Syntax
import Z3.Monad

--
guessInvariant :: (Sort, Args, [AST], Fields, SSAMap) -> Int -> Exp  -> AST -> Z3 AST
guessInvariant (objSort, pars, res, fields, ssamap) pid cond pre =
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
    _ -> return Nothing -- error $ "generalizeCond: " ++ show _cond