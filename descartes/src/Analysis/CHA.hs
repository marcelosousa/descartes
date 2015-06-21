{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.CHA (Class Hierarchy Analysis)
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.CHA (cha) where

import Language.Java.Parser
import Language.Java.Syntax
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Debug.Trace as T
import Control.Monad.State.Strict
import Analysis.Types

trace a b = b

type CHAState = AState ()

iCHA = Env M.empty M.empty

-- main analysis function
cha :: CompilationUnit -> Env
cha (CompilationUnit _ _ classdecls) = 
    snd $ runState (_cha classdecls) iCHA

class CHA a where
    _cha :: a -> CHAState
    
instance CHA [TypeDecl] where
  _cha [] = return ()
  _cha (c:cs) = _cha c >> _cha cs
  
instance CHA TypeDecl where
  _cha tydec = 
    case tydec of
      ClassTypeDecl cldec -> _cha cldec
      InterfaceTypeDecl _ -> undefined

instance CHA ClassDecl where
  _cha cldec = 
    case cldec of
      ClassDecl modifiers ident typeparams super interfs body -> undefined
      _ -> undefined
      

