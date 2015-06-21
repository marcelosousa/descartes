module Analysis.Verifier where

import Z3.Monad
import Language.Java.Syntax
import qualified Data.Map as M
import Data.Map (Map)

import Analysis.Types

type VerState = AState Bool

iVer = Env M.empty M.empty

-- Total Order Checker
isTotalOrder :: Comparator -> VerState
isTotalOrder comp = do 
    antisym <- isAntiSymmetric comp
    trans   <- isTransitive comp
    total   <- isTotal comp
    return $ antisym && trans && total
    
-- | Anti-symmetry Checker
isAntiSymmetric :: Comparator -> VerState
isAntiSymmetric = undefined

-- | Transitivity Checker
isTransitive :: Comparator -> VerState
isTransitive (Comp cname mdecl) = 
  case mdecl of
    MethodDecl mods typar mty ident pars exty body -> do
        undefined
    _ -> error "isTransitive: not allowed"
--    MethodDecl      [Modifier] [TypeParam] (Maybe Type) Ident [FormalParam] [ExceptionType] MethodBody = undefined

-- | Totality
isTotal :: Comparator -> VerState
isTotal = undefined

{-
MemberDecl (MethodDecl [Public] [] (Just (PrimType IntT)) (Ident "compare") 
        [FormalParam [] (RefType (ClassRefType (ClassType [(Ident "Test01",[])]))) False (VarId (Ident "o1")),
         FormalParam [] (RefType (ClassRefType (ClassType [(Ident "Test01",[])]))) False (VarId (Ident "o2"))
        ] 
        
        [] 


(MethodBody (Just (Block [BlockStmt (IfThenElse (BinOp (ExpName (Name [Ident "o1",Ident "x"])) LThan (ExpName (Name [Ident "o2",Ident "x"]))) (Return (Just (PreMinus (Lit (Int 1))))) (IfThenElse (BinOp (ExpName (Name [Ident "o1",Ident "x"])) GThan (ExpName (Name [Ident "o2",Ident "x"]))) (Return (Just (Lit (Int 1)))) (IfThenElse (BinOp (ExpName (Name [Ident "o1",Ident "y"])) LThan (ExpName (Name [Ident "o2",Ident "y"]))) (Return (Just (PreMinus (Lit (Int 1))))) (IfThenElse (BinOp (ExpName (Name [Ident "o1",Ident "y"])) GThan (ExpName (Name [Ident "o2",Ident "y"]))) (Return (Just (Lit (Int 1)))) (Return (Just (Lit (Int 0))))))))]))))]))]
-}