{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Main where

import Data.Maybe 

import Language.Java.Parser hiding (opt)
import Language.Java.Syntax
import Language.Java.Pretty hiding (opt)

import System.Console.CmdArgs hiding (opt)
import System.FilePath.Posix
--import System.FilePath.Find
import System.Directory
import qualified Debug.Trace as T

import Analysis.CHA
import Analysis.Verifier
import Analysis.Util
import Analysis.Types
import Analysis.Consolidation
import Analysis.Properties

import Z3.Monad

--trace a b = b

_program, _summary :: String
_summary = unlines ["descartes - v0.1","Cartersian Hoare Logic Verifier.","Copyright 2015 @ Marcelo Sousa"]
_program = "descartes"
_help    = "The input parameter is a Java project directory."

data Property = P1 | P2 | P3
  deriving (Show, Data, Typeable, Eq)
  
data Option = Verify {input :: FilePath, prop :: Int, mode :: Int, logLevel :: Int}
  deriving (Show, Data, Typeable, Eq)

verifyMode :: Option
verifyMode = Verify {input = def &= args
                    ,prop = def
                    ,mode = def
                    ,logLevel = def} &= help "verify a input Java file"

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [verifyMode]
         &= help _help
         &= program _program
         &= summary _summary

-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options

runOption :: Option -> IO ()
runOption (Verify path p mode logLevel) = descartes_main logLevel mode path (arity p) (toProp p) (showProp p)
--runOption (Verify path) = find always (extension ==? ".java") path >>=
--                          mapM_ frontend
  
arity :: Int -> Int
arity 1 = 2 
arity 2 = 3
arity 3 = 3

toProp :: Int -> Prop
toProp 1 = prop1
toProp 2 = prop2
toProp 3 = prop3

showProp :: Int -> String
showProp 1 = "Property 1: forall x and y, sgn(compare(x,y)) == −sgn(compare(y,x))"
showProp 2 = "Property 2: for all x, y and z, compare(x, y) > 0 and compare(y, z) > 0 implies compare(x, z) > 0."
showProp 3 = "Property 3: for all x, y and z, compare(x,y) == 0 implies that sgn(compare(x, z)) == sgn(compare(y, z))."
    
descartes_main :: Int -> Int -> FilePath -> Int -> Prop -> String -> IO ()
descartes_main logLevel mode file arity prop propName = do 
  ast <- parser compilationUnit `fmap` readFile file 
  case ast of 
    Left e -> print $ file ++ ": " ++ show e
    Right cu -> do
        let classMap = getInfo cu
            comps = getComps cu
            comparators = map (\c -> map (\idx -> rename idx c) [1..arity]) comps
        if logLevel > 0
        then do 
            mapM_ (\cs -> mapM_ (\(Comp _ f) -> putStrLn $ prettyPrint f) cs) comparators
            descartes mode classMap (head comparators) prop propName
        else descartes mode classMap (head comparators) prop propName

descartes mode classMap comparator prop propName = do 
        (vals, models) <- 
            case mode of 
                0 -> evalZ3 $ verify True classMap comparator prop
                1 -> evalZ3 $ verify False classMap comparator prop
                2 -> evalZ3 $ verifyWithSelf classMap comparator prop
        case vals of
            Unsat -> putStrLn "Unsat"
            Sat -> do
                putStrLn $ "Comparator is buggy! " ++ propName ++ " fails!\nCounter-example:"
                putStrLn $ fromJust models

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

