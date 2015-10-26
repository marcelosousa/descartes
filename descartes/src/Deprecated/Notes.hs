test = evalZ3With Nothing opts testQE >>= \mbSol ->
         case mbSol of
           Nothing  -> error "No solution found."
           Just sol -> putStr "Solution: " >> print sol
  where opts = opt "MODEL" True -- +? opt "ELIM_QUANTIFIERS" True

testQE :: Z3 (Maybe String)
testQE = do
    pars <- mkParams
    str <- paramsToString pars
    error $ str
    isort <- mkIntSort
    xSym <- mkStringSymbol "x"
    x <- mkConst xSym isort
    xApp <- toApp x
    ySym <- mkStringSymbol "y"
    y <- mkConst ySym isort
    yApp <- toApp y
    i0 <- mkIntNum 0    
    cond1 <- mkEq x i0
    cond2 <- mkEq y i0
    -- cond1 && cond2
    body <- mkAnd [cond1, cond2]
    fml <- mkExistsConst [] [xApp] body
    assert fml
    fmap snd $ withModel $ \m -> showModel m

{-
ttest_main = evalZ3 ttest

ttest :: Z3 String
ttest = do 
    isort <- mkIntSort
    xSym <- mkStringSymbol "x"
    x <- mkVar 
    y <- mkFreshFuncDecl "y" [] isort
    i0 <- mkIntNum 0    
    ast <- mkEq x i0
    ast' <- replaceVariable "x" y ast
    astToString ast'
-}
