-------------------------------------------------------------------------------
-- Module    :  Analysis.Consolidation
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Consolidation where

import Z3.Monad

import Data.Map (Map)
import Control.Monad.State.Strict
import Language.Java.Syntax

import Analysis.Types

type ConState = AState Comparator

consolidate :: Comparator -> Comparator -> ConState
consolidate (Comp c1 m1) (Comp c2 m2) = undefined




mkMyIntSort :: Z3 Sort
mkMyIntSort = do
    myint <- mkStringSymbol "MyInt"
    mkUninterpretedSort myint

run :: IO ()
run = do 
   evalZ3 funcScript >>= print
   evalZ3 test >>= print
   
--test :: Z3     
mytest :: Z3 Result 
mytest = do
    intSort <- mkIntSort
    myIntSort <- mkStringSymbol "MyInt" >>= mkUninterpretedSort 
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
    myintSort <- mkMyIntSort
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

    