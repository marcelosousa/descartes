{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Main where

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty

import System.Console.CmdArgs
import System.FilePath.Posix
import System.FilePath.Find
import System.Directory
import qualified Debug.Trace as T

import Analysis.CHA
import Analysis.Verifier
import Analysis.Util
import Analysis.Types
import Analysis.Consolidation

import Z3.Monad

trace a b = b

_program, _summary :: String
_summary = unlines ["descartes - v0.1","Cartersian Hoare Logic Verifier.","Copyright 2015 @ Marcelo Sousa"]
_program = "descartes"
_help    = "The input parameter is a Java project directory."

data Option = Verify {input :: FilePath}
  deriving (Show, Data, Typeable, Eq)

verifyMode :: Option
verifyMode = Verify {input = def &= args} &= help "verify a input Java file"

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
runOption (Verify path) = find always (extension ==? ".java") path >>=
                          mapM_ frontend
  
frontend :: FilePath -> IO ()
frontend file = do 
  ast <- parser compilationUnit `fmap` readFile file 
  case ast of 
    Left e -> print $ file ++ ": " ++ show e
    Right cu -> do
        let classMap = getInfo cu
            comps = getComps cu
            comparators = map (\c -> map (\idx -> rename idx c) [1,2,3]) comps
        mapM_ (\cs -> mapM_ (\(Comp _ f) -> putStrLn $ prettyPrint f) cs) comparators
        print classMap
        vals <- evalZ3 $ verify classMap (head comparators) transitivity
        print vals
--        print comps
--        print comps1
--        
