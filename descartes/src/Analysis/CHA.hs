{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.CHA (Class Hierarchy Analysis)
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.CHA where

import Language.Java.Parser
import Language.Java.Syntax
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Debug.Trace as T

trace a b = b

-- Class Name SuperClass? Subclasses Interfaces_Implementations Methods
data Class = Class 
  { name  :: Ident
  , super :: Maybe Ident
  , subcs :: [Ident] 
  , inter :: [Ident]
  , meths :: [Ident]
  }
  deriving (Show,Ord,Eq)
    
type CHA = Map Ident Class

-- main analysis function
cha :: CompilationUnit -> CHA
cha = undefined

