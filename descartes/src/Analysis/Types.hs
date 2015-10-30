{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Types
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Types where

import Control.Monad.State.Strict
import Control.Monad.ST.Safe
import Data.Map (Map)
import Language.Java.Syntax
import Z3.Monad hiding (Params)

import qualified Data.Map as M

-- receives the parameters and returns to specify the pre and post-condition
-- need to use maps for the parameters, returns, fields
type Params = Map Ident AST
type Res  = [AST]
type Fields = Map Ident FuncDecl
type Prop = (Params, Res, Fields) -> Z3 (AST, AST)

-- SSA map to build a simple SSA representation on the fly
type SSAMap = Map Ident (AST, Sort, Int)

-- We need the assign map to understand the value of the loop counter
type AssignMap = Map Ident Exp

data Env = Env
  { _objSort :: Sort
  , _params  :: Params
  , _res     :: Res
  , _fields  :: Fields
  , _ssamap  :: SSAMap
  , _axioms  :: AST
  , _pre     :: AST
  , _post    :: AST
  , _opt     :: Bool
  , _debug   :: Bool
  , _fuse    :: Bool
  }

type EnvOp a = StateT Env Z3 a

_default = (Unsat,Nothing)

-- @ update the pre-condition
updatePre :: AST -> EnvOp ()
updatePre pre = do
  s@Env{..} <- get
  put s{ _pre = pre}

-- @ update the pre-condition
updatePost :: AST -> EnvOp ()
updatePost post = do
  s@Env{..} <- get
  put s{ _post = post}

-- @ update the ssa map
updateSSAMap :: SSAMap -> EnvOp ()
updateSSAMap ssamap = do
  s@Env{..} <- get
  put s{ _ssamap = ssamap}
  
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

-- | Front-end Environment
data FEEnv = FEEnv 
  {
    classes :: ClassMap
  , classgraph :: ClassGraph
  }
  deriving (Show, Ord, Eq)
  
-- | State of Analysis
type ConState = AState Comparator
type AState a = State FEEnv a

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