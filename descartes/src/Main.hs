{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Main where

import Analysis.Consolidation
import Analysis.Product
import Analysis.Properties
import Analysis.SelfComposition
import Analysis.Types
import Analysis.Util
import Data.Maybe
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import System.Console.CmdArgs hiding (opt)
import System.Directory
import System.FilePath.Posix
import Z3.Monad

import qualified Debug.Trace as T

_program, _summary :: String
_summary = unlines ["descartes - v0.1","Cartersian Hoare Logic Verifier.","Copyright 2015 @ Marcelo Sousa"]
_program = "descartes"
_help    = "The input parameter is a Java project directory."

data Property = P1 | P2 | P3
  deriving (Show, Data, Typeable, Eq)
  
data Option =
  Verify {input :: FilePath
         ,prop :: Int
         ,mode :: Int
         ,logLevel :: Int}
  deriving (Show, Data, Typeable, Eq)

verifyMode :: Option
verifyMode =
  Verify {input = def &= args
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
runOption (Verify path p mode logLevel) =
  descartes_main logLevel mode path (arity p) (toProp p) (showProp p)

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

front_end :: FilePath -> IO ()
front_end file = do
  ast <- parser compilationUnit `fmap` readFile file 
  case ast of 
    Left e -> print $ file ++ ": " ++ show e
    Right cu -> print cu
    
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
  (vals, models) <- case mode of 
    0 -> evalZ3 $ verify True classMap comparator prop
    1 -> evalZ3 $ verify False classMap comparator prop
    2 -> evalZ3 $ verifyWithSelf classMap comparator prop
    3 -> evalZ3 $ verifyWithProduct classMap comparator prop
  case vals of
    Unsat -> putStrLn $ "Unsat: Comparator OBEYS " ++ propName
    Sat -> do
      putStrLn $ "Sat: Comparator VIOLATES " ++ propName --is buggy! " ++ propName ++ " fails!\nCounter-example:"
      putStrLn $ fromJust models