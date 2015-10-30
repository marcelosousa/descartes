{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Hoare
-- Copyright :  (c) 2015 Marcelo Sousa
-- Implements a strong-conditional calculus
-------------------------------------------------------------------------------
module Analysis.Hoare where

import Analysis.Engine
import Analysis.Axioms
import Analysis.Properties
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Data.Map (Map)
import Data.Maybe
import Language.Java.Pretty
import Language.Java.Syntax
import Z3.Monad hiding (Params)

import qualified Data.Map as M

-- Analyse Statements
-- Analyse Assume
assume :: Exp -> EnvOp ()
assume expr = do
 env@Env{..} <- get
 exprEnc <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) expr
 pre <- lift $ mkAnd [_pre,exprEnc]
 updatePre pre

-- Analyse Return
ret :: Int -> Maybe Exp -> EnvOp ()
ret pid mexpr = do
 env@Env{..} <- get
 case mexpr of
  Nothing -> error "analyser: return Nothing"
  Just expr -> do
   exprPsi <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) expr
   let resPid = _res !! pid
   r <- lift $ mkEq resPid exprPsi
   pre <- lift $ mkAnd [_pre,r]
   updatePre pre

-- Analyse Method Call
method_call :: MethodInvocation -> EnvOp ()
method_call minv =
 case minv of 
  MethodCall (Name [Ident "assume"]) [expr] -> do 
   env@Env{..} <- get
   expAST <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) expr
   pre <- lift $ mkAnd [_pre,expAST]
   updatePre pre
  _ -> error $ show minv ++ " not supported"
 
-- Analyse Assign
assign :: Exp -> Lhs -> AssignOp -> Exp -> EnvOp ()
assign _exp lhs aOp rhs = do
 env@Env{..} <- get
 rhsAst <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) rhs
 case lhs of
  NameLhs (Name [ident@(Ident str)]) -> do
   let (plhsAST,sort, i) = safeLookup "Assign" ident _ssamap
       cstr = str ++ show i
       ni = i+1
       nstr = str ++ show ni
   sym <- lift $ mkStringSymbol nstr
   var <- lift $ mkFreshFuncDecl nstr [] sort
   astVar <- lift $ mkApp var []
   let ssamap = M.insert ident (astVar, sort, ni) _ssamap
   ass <- lift $ processAssign astVar aOp rhsAst plhsAST
   pre <- lift $ mkAnd [_pre, ass]
   post <- lift $ replaceVariable cstr var _post
   updatePre pre
   updatePost post
   updateSSAMap ssamap
  _ -> error $ "Assign " ++ show _exp ++ " not supported"
  
-- Analyse Post De/Increment
postOp :: Exp -> Exp -> Op -> String -> EnvOp ()
postOp _exp lhs op str = do
 env@Env{..} <- get
 rhsAst <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) (BinOp lhs op (Lit $ Int 1))
 case lhs of
  ExpName (Name [ident@(Ident str)]) -> do
   let (plhsAST,sort, i) = safeLookup "Assign" ident _ssamap
       cstr = str ++ show i
       ni = i+1
       nstr = str ++ show ni
   sym <- lift $ mkStringSymbol nstr
   var <- lift $ mkFreshFuncDecl nstr [] sort
   astVar <- lift $ mkApp var []
   let ssamap = M.insert ident (astVar, sort, ni) _ssamap
   ass <- lift $ processAssign astVar EqualA rhsAst plhsAST
   pre <- lift $ mkAnd [_pre, ass]
   post <- lift $ replaceVariable cstr var _post
   updatePre pre
   updatePost post
   updateSSAMap ssamap
  _ -> error $ str ++ show _exp ++ " not supported"