{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Main where

import Language.Java.Parser
import Language.Java.Syntax
import System.Console.CmdArgs
import System.FilePath.Posix
import System.Directory
import Data.List
import qualified Debug.Trace as T
import Analysis.CHA

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
runOption (Verify f) = do
  frontend f
  
frontend :: FilePath -> IO ()
frontend file = do 
  ast <- parser compilationUnit `fmap` readFile file 
  case ast of 
    Left e -> print $ file ++ ":" ++ show e
    Right cu -> print cu

