-------------------------------------------------------------------------------
-- Module    :  Analysis.Properties
-- Copyright :  (c) 2015 Marcelo Sousa
-- Defines the properties of comparators:
--  https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html
-------------------------------------------------------------------------------

module Analysis.Properties where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Java.Syntax

import Z3.Monad

import Analysis.Types
import Analysis.Util

-- receives the parameters and returns to specify the pre and post-condition
-- need to use maps for the parameters, returns, fields
type Args = Map Ident AST
type Res  = [AST]
type Fields = Map Ident FuncDecl

type Prop = (Args, Res, Fields) -> Z3 (AST, AST)


prop1 :: Prop
prop1 (args, [res1,res2], fields) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
    pre <- mkAnd [eq1,eq2]
    i0 <- mkIntNum (0 :: Integer)
    -- compare(x,y) = 0 iff compare (y,x) = 0 
    r1eq <- mkEq res1 i0
    r2eq <- mkEq res2 i0
    poseq <- mkIff r1eq r2eq
    -- compare(x,y) > 0 iff compare(y,x) < 0
    r1gt <- mkGt res1 i0
    r2lt <- mkLt res2 i0
    posas <- mkIff r1gt r2lt
    pos <- mkAnd [poseq,posas]
    return (pre, pos)

-- transitivity
prop2 :: Prop
prop2 (args, [res1,res2,res3], fields) = do
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
    -- compare(x,y) > 0 and compare(y,z) > 0 => compare(x,z) > 0
    r1gt <- mkGt res1 i0
    r2gt <- mkGt res2 i0
    r3gt <- mkGt res3 i0
    r12gt <- mkAnd [r1gt,r2gt]
    pos <- mkImplies r12gt r3gt
    return (pre, pos)

prop3 :: Prop 
prop3 (args, [res1,res2,res3], fields) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o13 = safeLookup "trans" (Ident "o13") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
        o23 = safeLookup "trans" (Ident "o23") args    
    eq1 <- mkEq o11 o12
    eq2 <- mkEq o21 o13
    eq3 <- mkEq o22 o23
    pre <- mkAnd [eq1,eq2,eq3]
    i0 <- mkIntNum (0 :: Integer)
    -- compare(x,y) = 0
    r1 <- mkEq res1 i0
    -- compare(x,z) > 0 iff compare (y,z) > 0
    r2gt <- mkGt res2 i0
    r3gt <- mkGt res3 i0
    posgt <- mkIff r2gt r3gt
    -- compare(x,z) = 0 iff compare (y,z) = 0
    r2eq <- mkEq res2 i0
    r3eq <- mkEq res3 i0
    poseq <- mkIff r2eq r3eq    
    pos' <- mkAnd [posgt,poseq]
    pos <- mkImplies r1 pos'
    return (pre, pos)
    
-- transitivity
-- forall (o11,o21,o12,o22,o13,o23). 
--      o11 = o13 and o12 = o21 and o22 = o23
--  =>  ((res1 > 0 and res2 > 0) => res3 > 0)
transitivity :: Prop
transitivity (args, [res1,res2,res3], fields) = do
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
    -- compare(x,y) = 0 and compare (y,z) = 0 => compare(x,z) = 0
    r1eq <- mkEq res1 i0
    r2eq <- mkEq res2 i0
    r3eq <- mkEq res3 i0
    r12eq <- mkAnd [r1eq,r2eq]
    poseq <- mkImplies r12eq r3eq
    -- compare(x,y) < 0 and compare(y,z) < 0 => compare(x,z) < 0
    r1lt <- mkLt res1 i0
    r2lt <- mkLt res2 i0
    r3lt <- mkLt res3 i0
    r12lt <- mkAnd [r1lt,r2lt]
    poslt <- mkImplies r12lt r3lt
    -- compare(x,y) > 0 and compare(y,z) > 0 => compare(x,z) > 0
    r1gt <- mkGt res1 i0
    r2gt <- mkGt res2 i0
    r3gt <- mkGt res3 i0
    r12gt <- mkAnd [r1gt,r2gt]
    posgt <- mkImplies r12gt r3gt
    pos <- mkAnd [poseq,poslt,posgt]
    return (pre, pos)

antisymmetry :: Prop
antisymmetry (args, [res1, res2], fields) = do
    let o11 = safeLookup "antisymmetry" (Ident "o11") args
        o12 = safeLookup "antisymmetry" (Ident "o12") args
        o21 = safeLookup "antisymmetry" (Ident "o21") args
        o22 = safeLookup "antisymmetry" (Ident "o22") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
   -- neq1 <- mkNot =<< mkEq o11 o21
   -- lneqs <- mapM (\field -> mkFieldApp o11 o21 field fields) $ M.keys fields
    pre <- mkAnd $ [eq1,eq2] --,neq1] ++ lneqs
    i0 <- mkIntNum (0 :: Integer)
    r1 <- mkLt res1 i0
    r2 <- mkLt res2 i0
    r12 <- mkOr [r1,r2]
    r3 <- mkGt res1 i0
    r4 <- mkGt res2 i0
    r34 <- mkOr [r3,r4]
--    pos <- mkAnd [r12, r34]
    pos <- mkIff r3 r2
    return (pre,pos)

equality :: Prop
equality (args, [res1], fields) = do
    let o11 = safeLookup "eq" (Ident "o11") args
        o21 = safeLookup "eq" (Ident "o21") args
    eq1 <- mkEq o11 o21
    lneqs <- mapM (\field -> mkFieldApp o11 o21 field fields) $ M.keys fields
    pre <- mkAnd $ eq1:lneqs
    i0 <- mkIntNum (0 :: Integer)
    pos <- mkEq res1 i0
    return (pre,pos)

mkFieldApp o1 o2 field fields = do
    let fn = safeLookup "processName: Field"  field fields
    o1app <- mkApp fn [o1]
    o2app <- mkApp fn [o2]
    mkNot =<< mkEq o1app o2app
