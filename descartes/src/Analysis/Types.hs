-------------------------------------------------------------------------------
-- Module    :  Analysis.Types
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Types where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Strict
import Language.Java.Syntax

-- | ClassMap: Map Identifier ClassDeclaration
type ClassMap = Map String ClassInfo

getFieldNames :: String -> ClassMap -> [String]
getFieldNames str cInfo = 
    case M.lookup str cInfo of
        Nothing -> []
        Just (ClassInfo _ fields) -> foldl (\acc f -> getFieldName f ++ acc) [] fields

getFieldName :: MemberDecl -> [String]
getFieldName mDecl = case mDecl of
    FieldDecl _ _ varDecl -> foldl (\acc f -> getName f ++ acc) [] varDecl
    _ -> []

getName :: VarDecl -> [String]
getName (VarDecl (VarId (Ident str)) _) = [str]
getName _ = []
    
-- | CHA Graph: Class Hierarchy Graph
type ClassGraph = Map Ident ([Ident],[Ident])

-- | Analysis Environment
data Env = Env 
  {
    classes :: ClassMap
  , classgraph :: ClassGraph
  }
  deriving (Show, Ord, Eq)
  
-- | State of Analysis
type AState a = State Env a

data Comparator = Comp
  {
    className :: Ident
  , method    :: MemberDecl
  }
  deriving (Show, Ord, Eq)
  
type Comparators = [Comparator]

type Substitutions = Map Ident Ident

data ClassInfo = ClassInfo
  {
    name :: Ident
  , fields :: [MemberDecl]
  }
  deriving (Show, Eq, Ord)