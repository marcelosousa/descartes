{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Main where

import Data.Maybe 

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty

import System.Console.CmdArgs
import System.FilePath.Posix
--import System.FilePath.Find
import System.Directory
import qualified Debug.Trace as T

import Analysis.CHA
import Analysis.Verifier
import Analysis.Util
import Analysis.Types
import Analysis.Consolidation
import Analysis.Props

import Z3.Monad

trace a b = b

_program, _summary :: String
_summary = unlines ["descartes - v0.1","Cartersian Hoare Logic Verifier.","Copyright 2015 @ Marcelo Sousa"]
_program = "descartes"
_help    = "The input parameter is a Java project directory."

data Property = P1 | P2 | P3
  deriving (Show, Data, Typeable, Eq)
  
data Option = Verify {input :: FilePath, prop :: Int}
  deriving (Show, Data, Typeable, Eq)

verifyMode :: Option
verifyMode = Verify {input = def &= args
                    ,prop = def} &= help "verify a input Java file"

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
runOption (Verify path p) = descartes path (arity p) (toProp p) (show p)
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

descartes :: FilePath -> Int -> Prop -> String -> IO ()
descartes file arity prop propName = do 
  ast <- parser compilationUnit `fmap` readFile file 
  case ast of 
    Left e -> print $ file ++ ": " ++ show e
    Right cu -> do
        let classMap = getInfo cu
            comps = getComps cu
            comparators = map (\c -> map (\idx -> rewrite $ rename idx c) [1..arity]) comps
        mapM_ (\cs -> mapM_ (\(Comp _ f) -> putStrLn $ prettyPrint f) cs) comparators
--        print cu
--        print classMap
        (vals, models) <- evalZ3 $ verify classMap (head comparators) prop
        case vals of
            Unsat -> print "Unsat"
            Sat -> do
                putStrLn $ propName ++ " Comparator is buggy!: Counter example:"
                putStrLn $ fromJust models
--        print comps
--        print comps1
--        
