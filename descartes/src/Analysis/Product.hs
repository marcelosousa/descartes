{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Product
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Product (verifyWithProduct) where

import Analysis.Axioms
import Analysis.Engine
import Analysis.Hoare
import Analysis.Invariant
import Analysis.Properties
import Analysis.Util
import Analysis.Types

import Control.Monad.State.Strict
import Control.Monad.ST.Safe

import Data.Map (Map)
import Data.Maybe

import Language.Java.Pretty
import Language.Java.Syntax

import System.IO.Unsafe
import Z3.Monad

import qualified Data.Map as M
import qualified Debug.Trace as T

verifyWithProduct :: ClassMap -> [Comparator] -> Prop -> Z3 (Result,Maybe String)
verifyWithProduct classMap _comps prop = do
 let comps = _comps
 (objSort, pars, res, fields) <- prelude classMap comps
 (pre, post) <- prop (pars, res, fields)
 (fields', axioms) <- addAxioms objSort fields
 let blocks = zip [0..] $ getBlocks comps
 iSSAMap <- getInitialSSAMap
 let iEnv = Env objSort pars res fields' iSSAMap axioms pre post post False False False 0
 ((res, mmodel),_) <- runStateT (analyser blocks) iEnv
 case res of 
  Unsat -> return (Unsat, Nothing)
  Sat -> do
   str <- showModel $ fromJust mmodel
   return (Sat, Just str)
        
-- strongest post condition
_triple :: String -> String -> String -> String
_triple pre stm post =
 unlines
  ["-----------------"
  ,"Product - Analyser State"
  ,pre
  ,stm
  ,post
  ,"-----------------"]

-- @ Analyser main function
analyser :: [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyser stmts = do
 env@Env{..} <- get
 if _debug
 then analyser_debug stmts
 else analyse stmts

analyser_debug :: [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyser_debug stmts = do
 env@Env{..} <- get
 preStr  <- lift $ astToString _pre
 postStr <- lift $ astToString _post
 case stmts of
  [] -> do 
   let k = T.trace (_triple preStr "end" postStr) $ unsafePerformIO $ getChar
   k `seq` analyse stmts
  ((pid,Block []):rest) -> analyse stmts
  ((pid,Block (bstmt:r1)):rest) -> do
   let k = T.trace (_triple preStr (prettyPrint bstmt) postStr) $ unsafePerformIO $ getChar
   k `seq` analyse stmts

analyse :: [(Int,Block)] -> EnvOp (Result,Maybe Model)   
analyse stmts = do
 env@Env{..} <- get
 case stmts of
  [] -> if _numret == length _res
        then lift $ local $ helper _axioms _pre _post
        else lift $ local $ helper _axioms _pre _invpost
  ((pid,Block []):rest) -> analyser []
--    if _embed
--    then analyser rest -- change here
  ((pid,Block (bstmt:r1)):rest) -> case bstmt of
   BlockStmt stmt -> analyser_stmt stmt (pid, Block r1) rest 
   LocalVars mods ty vars -> do
    sort <- lift $ processType ty    
    (nssamap, npre) <- 
      lift $ foldM (\(ssamap', pre') v -> 
        processNewVar (_objSort,_params,_res,_fields,ssamap',pre') sort v 1) (_ssamap, _pre) vars
    updatePre npre
    updateSSAMap nssamap
    analyser ((pid, Block r1):rest)

analyser_stmt :: Stmt -> (Int,Block) -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyser_stmt stmt (pid, Block r1) rest =
 case stmt of
  StmtBlock (Block block) -> analyser ((pid, Block (block ++ r1)):rest)
  Assume expr -> do
   assume expr
   analyser ((pid, Block r1):rest)
  Return mexpr -> do
   ret pid mexpr
   env@Env{..} <- get
   updateNumRet
   analyser rest
  IfThen cond s1 -> do
   let ifthenelse = IfThenElse cond s1 (StmtBlock (Block []))
   analyser_stmt ifthenelse (pid, Block r1) rest
  IfThenElse cond s1 s2 -> analyse_conditional pid r1 rest cond s1 s2 
  ExpStmt expr -> analyse_exp pid ((pid,Block r1):rest) expr
  While _cond _body -> analyse_loop pid r1 rest _cond _body

-- Analyse Expressions
analyse_exp :: Int -> [(Int, Block)] -> Exp -> EnvOp (Result, Maybe Model)
analyse_exp pid rest _exp =
 case _exp of
  MethodInv minv -> do 
   method_call minv
   analyser rest
  Assign lhs aOp rhs -> do
   assign _exp lhs aOp rhs
   analyser rest 
  PostIncrement lhs -> do
   postOp _exp lhs Add "PostIncrement"
   analyser rest
  PostDecrement lhs -> do
   postOp _exp lhs Sub "PostDecrement"
   analyser rest

-- Analyse If Then Else
analyse_conditional :: Int -> [BlockStmt] -> [(Int,Block)] -> Exp -> Stmt -> Stmt -> EnvOp (Result,Maybe Model)
analyse_conditional pid r1 rest cond s1 s2 =
 if cond == Nondet
 then do
  env <- get
  resThen <- analyser ((pid, Block (BlockStmt s1:r1)):rest)                
  put env
  resElse <- analyser ((pid, Block (BlockStmt s2:r1)):rest)
  combine resThen resElse                
 else do
  env@Env{..} <- get
  condSmt <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) cond
  -- then branch
  preThen <- lift $ mkAnd [_pre, condSmt]
  resThen <- analyse_branch preThen s1
  -- else branch
  put env
  ncondSmt <- lift $ mkNot condSmt
  preElse <- lift $ mkAnd [_pre, ncondSmt]
  resElse <- analyse_branch preElse s2
  combine resThen resElse
 where
   analyse_branch phi branch = do
    env@Env{..} <- get
    updatePre phi
    let r = ((pid, Block (BlockStmt branch:r1)):rest)
    analyser r
   combine :: (Result, Maybe Model) -> (Result, Maybe Model) -> EnvOp (Result, Maybe Model)
   combine (Unsat,_) (Unsat,_) = return _default
   combine (Unsat,_) res = return res
   combine res _ = return res

-- Analyse Loops
analyse_loop :: Int -> [BlockStmt] -> [(Int,Block)] -> Exp -> Stmt -> EnvOp (Result,Maybe Model)
analyse_loop pid r1 rest _cond _body =  do
 let bstmt = BlockStmt $ While _cond _body
 env@Env{..} <- get
 invs <- guessInvariants (pid+1) _cond
 analyse_loop_w_inv invs
 where
   analyse_loop_w_inv [] = error "analyse_loop failed"
   analyse_loop_w_inv (inv:is) = do
    it_res <- _analyse_loop rest pid _cond _body inv
    if it_res
    then do
     env@Env{..} <- get
     pre <- lift $ mkAnd [inv,_pre]
     updatePre pre
     analyser ((pid,Block r1):rest)
    else analyse_loop_w_inv is
   
--
_analyse_loop :: [(Int,Block)] -> Int -> Exp -> Stmt -> AST -> EnvOp Bool
_analyse_loop rest pid _cond _body inv = do
 invStr  <- lift $ astToString inv
 env@Env{..} <- trace ("Invariant:\n" ++ invStr) $ get
 (checkPre,_) <- lift $ local $ helper _axioms _pre inv
 case checkPre of
  Unsat -> do
   condAst <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) _cond
   ncondAst <- lift $ mkNot condAst
   (checkInv,_) <- lift $ mkAnd [inv,ncondAst] >>= \npre -> local $ helper _axioms npre inv
   case checkInv of
    Unsat -> do
     pre <- lift $ mkAnd [inv,condAst]
    -- post <- lift $ mkOr [inv,_post]
     let s = (pid, Block [BlockStmt _body])
     updatePre pre
     updateInvPost inv -- post
     (bodyCheck,m) <- analyser (s:rest)
     case bodyCheck of
      Unsat -> return True
      Sat -> do
       put env
       return False -- {inv && pre} body {inv} failed
    Sat -> return False -- inv && not_cond =/=> inv
  Sat -> return False -- pre =/=> inv

