-------------------------------------------------------------------------------
-- Module    :  Analysis.Axioms
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------

module Analysis.Axioms where


import Analysis.Properties
import Analysis.Types
import Language.Java.Syntax
import Z3.Monad

import qualified Data.Map as M

addAxioms :: Sort -> Fields -> Z3 (Fields, AST)
addAxioms objSort fields = do
    iSort <- mkIntSort
    toLowerCase <- mkFreshFuncDecl "toLowerCase" [iSort] iSort
    toUpperCase <- mkFreshFuncDecl "toUpperCase" [iSort] iSort
    nondet <- mkFreshFuncDecl "nondet" [iSort] iSort
    fnDouble <- mkFreshFuncDecl "compareDouble" [iSort, iSort] iSort
    fnInt <- mkFreshFuncDecl "compareInt" [iSort, iSort] iSort
--    fnStr <- mkFreshFuncDecl "compareIgnoreCaseString" [objSort, objSort] iSort
    fnStr <- mkFreshFuncDecl "compareIgnoreCaseString" [iSort, iSort] iSort
    equals <- mkFreshFuncDecl "equals" [iSort, iSort] iSort
    let fields' = M.insert (Ident "toLowerCase") toLowerCase $ 
                  M.insert (Ident "toUpperCase") toUpperCase $
                  M.insert (Ident "nondet") nondet $ 
                  M.insert (Ident "compareDouble") fnDouble $ 
                  M.insert (Ident "compareInt") fnInt $ 
                  M.insert (Ident "compareIgnoreCaseString") fnStr $
                  M.insert (Ident "equals") equals fields
    -- add prop1 axiom for Double.compare
    p1AxiomDouble <- genP1Axiom iSort fnDouble
    -- add prop1 axiom for Int.compare
    p1AxiomInt <- genP1Axiom iSort fnInt    
    -- add prop1 axiom for String.compareIgnoreCase
    p1AxiomStr <- genP1Axiom iSort fnStr
    
    -- add prop2 axiom for Double.compare
    p2AxiomDouble <- genP2Axiom iSort fnDouble
    -- add prop2 axiom for Int.compare
    p2AxiomInt <- genP2Axiom iSort fnInt    
    -- add prop2 axiom for String.compareIgnoreCase
    p2AxiomStr <- genP2Axiom iSort fnStr
    -- add prop2 axiom for Equals
    p2AxiomEquals0 <- genP2Axiom' 0 iSort equals
    p2AxiomEquals1 <- genP2Axiom'' 0 iSort equals
    
    -- add prop3 axiom for Double.compare
    p3AxiomDouble <- genP3Axiom iSort fnDouble
    -- add prop3 axiom for Int.compare
    p3AxiomInt <- genP3Axiom iSort fnInt    
    -- add prop3 axiom for String.compareIgnoreCase
    p3AxiomStr <- genP3Axiom iSort fnStr

    -- add prop4 axiom for equals
    p4AxiomEquals <- genP4Axiom iSort equals
    
    -- add prop4 axiom for Double.compare
    --p4AxiomDouble <- genP4Axiom iSort fnDouble
    -- add prop4 axiom for Int.compare
    --p4AxiomInt <- genP4Axiom iSort fnInt    
    -- add prop4 axiom for String.compareIgnoreCase
    --p4AxiomStr <- genP4Axiom objSort fnStr
    
    axioms <- mkAnd [ p1AxiomDouble, p2AxiomDouble, p3AxiomDouble
                    , p1AxiomInt, p2AxiomInt, p3AxiomInt
                    , p1AxiomStr, p2AxiomStr, p3AxiomStr
                    , p2AxiomEquals0, p2AxiomEquals1, p4AxiomEquals
                    ]
    return (fields', axioms)

-- Generate prop1 axiom
genP1Axiom :: Sort -> FuncDecl -> Z3 AST
genP1Axiom sort fn = do    
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym sort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym sort
    yApp <- toApp y
    i0 <- mkIntNum 0
    -- cond1: compare(x,y) > 0 iff compare(y,x) < 0
    fnXYgt0 <- mkApp fn [x,y] >>= \a -> mkGt a i0
    fnYXlt0 <- mkApp fn [y,x] >>= \a -> mkLt a i0
    cond1 <- mkIff fnXYgt0 fnYXlt0
    -- cond2: compare(x,y) = 0 iff compare (y,x) = 0 
    fnXYeq0 <- mkApp fn [x,y] >>= \a -> mkEq a i0
    fnYXeq0 <- mkApp fn [y,x] >>= \a -> mkEq a i0
    cond2 <- mkIff fnXYeq0 fnYXeq0
    -- cond1 && cond2
    body <- mkAnd [cond1, cond2]
    mkForallConst [] [xApp, yApp] body

-- Generate prop2 axiom
genP2Axiom :: Sort -> FuncDecl -> Z3 AST
genP2Axiom sort fn = do    
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym sort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym sort
    yApp <- toApp y
    zSym <- mkStringSymbol "z"
    z <- mkConst zSym sort
    zApp <- toApp z
    i0 <- mkIntNum 0
    -- cond1: compare(x,y) > 0 
    cond1 <- mkApp fn [x,y] >>= \a -> mkGt a i0
    -- cond2: compare(y,z) > 0
    cond2 <- mkApp fn [y,z] >>= \a -> mkGt a i0
    -- cond3: compare(x,z) > 0
    cond3 <- mkApp fn [x,z] >>= \a -> mkGt a i0
    -- cond1 and cond2 implies cond3
    body <- mkAnd [cond1,cond2] >>= \pre -> mkImplies pre cond3
    mkForallConst [] [xApp, yApp, zApp] body

genP2Axiom' :: Int -> Sort -> FuncDecl -> Z3 AST
genP2Axiom' i sort fn = do    
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym sort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym sort
    yApp <- toApp y
    zSym <- mkStringSymbol "z"
    z <- mkConst zSym sort
    zApp <- toApp z
    i0 <- mkIntNum i
    -- cond1: equals(x,y) == 0
    cond1 <- mkApp fn [x,y] >>= \a -> mkEq a i0
    -- cond2: equals(y,z) == 0
    cond2 <- mkApp fn [y,z] >>= \a -> mkEq a i0
    -- cond3: equals(x,z) == 0
    cond3 <- mkApp fn [x,z] >>= \a -> mkEq a i0
    -- cond1 and cond2 implies cond3
    body <- mkAnd [cond1,cond2] >>= \pre -> mkImplies pre cond3
    mkForallConst [] [xApp, yApp, zApp] body

genP2Axiom'' :: Int -> Sort -> FuncDecl -> Z3 AST
genP2Axiom'' i sort fn = do    
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym sort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym sort
    yApp <- toApp y
    zSym <- mkStringSymbol "z"
    z <- mkConst zSym sort
    zApp <- toApp z
    i0 <- mkIntNum i
    -- cond1: equals(x,y) == 0
    cond1 <- mkApp fn [x,y] >>= \a -> mkEq a i0 >>= mkNot
    -- cond2: equals(y,z) == 0
    cond2 <- mkApp fn [y,z] >>= \a -> mkEq a i0 >>= mkNot
    -- cond3: equals(x,z) == 0
    cond3 <- mkApp fn [x,z] >>= \a -> mkEq a i0 >>= mkNot
    -- cond1 and cond2 implies cond3
    body <- mkAnd [cond1,cond2] >>= \pre -> mkImplies pre cond3
    mkForallConst [] [xApp, yApp, zApp] body
    
-- Generate prop3 axiom
genP3Axiom :: Sort -> FuncDecl -> Z3 AST
genP3Axiom sort fn = do    
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym sort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym sort
    yApp <- toApp y
    zSym <- mkStringSymbol "z"
    z <- mkConst zSym sort
    zApp <- toApp z
    i0 <- mkIntNum 0
    -- cond1: compare(x,y) = 0 
    cond1 <- mkApp fn [x,y] >>= \a -> mkEq a i0
    cond21 <- mkApp fn [x,z] >>= \a -> mkGt a i0
    -- cond21: compare(x,z) > 0
    cond21 <- mkApp fn [x,z] >>= \a -> mkGt a i0
    -- cond22: compare(y,z) > 0
    cond22 <- mkApp fn [y,z] >>= \a -> mkGt a i0
    -- cond2: cond21 iff cond22
    cond2 <- mkIff cond21 cond22
    -- cond31: compare(x,z) = 0
    cond31 <- mkApp fn [x,z] >>= \a -> mkEq a i0
    -- cond32: compare(y,z) = 0
    cond32 <- mkApp fn [y,z] >>= \a -> mkEq a i0
    -- cond3: cond31 iff cond32
    cond3 <- mkIff cond31 cond32
    -- cond1 implies (cond2 and cond3)
    body <- mkAnd [cond2,cond3] >>= \pos -> mkImplies cond1 pos
    mkForallConst [] [xApp, yApp, zApp] body

-- Generate prop4 axiom
genP4Axiom :: Sort -> FuncDecl -> Z3 AST
genP4Axiom sort fn = do    
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym sort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym sort
    yApp <- toApp y
    -- cond1: equals(x,y) == equals(y,x)
    fnXYeq1 <- mkApp fn [x,y] 
    fnYXeq1 <- mkApp fn [y,x] 
    body <- mkEq fnXYeq1 fnYXeq1
    mkForallConst [] [xApp, yApp] body
{-
genP4Axiom :: Sort -> FuncDecl -> Z3 AST
genP4Axiom sort fn = do    
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym sort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym sort
    yApp <- toApp y
    i0 <- mkIntNum 0
    -- cond1: compare(x,y) = 0 
    cond1 <- mkApp fn [x,y] >>= \a -> mkEq a i0
    cond2 <- mkEq x y
    body <- mkImplies cond1 cond2
    mkForallConst [] [xApp, yApp] body
-}