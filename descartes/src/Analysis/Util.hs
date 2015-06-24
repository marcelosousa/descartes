{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Util
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Util where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Strict
import Language.Java.Syntax

import Analysis.Types

--
safeLookup :: Ord k => String -> k -> Map k a -> a
safeLookup err k m = case M.lookup k m of
    Nothing -> error err
    Just a  -> a
    
class GetInfo a where
    getInfo :: a -> ClassMap

instance GetInfo CompilationUnit where
    getInfo (CompilationUnit _ _ typeDecls) = 
        foldl (\acc tyDecl -> M.union acc $ getInfo tyDecl) M.empty typeDecls

instance GetInfo TypeDecl where
    getInfo tyDecl = case tyDecl of
        ClassTypeDecl classDecl -> getInfo classDecl 
        InterfaceTypeDecl interDecl -> M.empty

instance GetInfo ClassDecl where
    getInfo clDecl = case clDecl of
        ClassDecl mods ident tyParms mRefTy refTys (ClassBody classBody) ->
            let fields = foldl (\acc c -> getFields c ++ acc) [] classBody
            in M.singleton (toString ident) (ClassInfo ident fields)
        EnumDecl  mods ident refTys enumBody -> M.empty

getFields :: Decl -> [MemberDecl]
getFields (MemberDecl mdecl) = getField mdecl
getFields (InitDecl _ _) = []

getField :: MemberDecl -> [MemberDecl]
getField mdecl = case mdecl of
    FieldDecl mods ty vardecls -> [mdecl]    
    _ -> []
    
data CompInter = Comparator | Comparable
  deriving (Show,Eq,Ord)

class GetComps a where
    getComps :: a -> Comparators

instance GetComps CompilationUnit where
    getComps (CompilationUnit _ _ typeDecls) = 
        foldl (\acc tyDecl -> getComps tyDecl ++ acc) [] typeDecls

instance GetComps TypeDecl where
    getComps tyDecl = case tyDecl of
        ClassTypeDecl classDecl -> getComps classDecl 
        InterfaceTypeDecl interDecl -> []

instance GetComps ClassDecl where
    getComps clDecl = case clDecl of
        ClassDecl mods ident tyParms mRefTy refTys (ClassBody classBody) ->
            let hasComp = implComp refTys -- this class declarator specifies that it implements one of the comparators interfaces
                comps = foldl (\acc c -> getComparators hasComp c ++ acc) [] classBody
            in map (\c -> Comp ident c) comps
        EnumDecl  mods ident refTys enumBody -> []

implComp :: [RefType] -> [CompInter]
implComp refTys = foldl (\a rty -> implComp' rty ++ a) [] refTys
  where implComp' (ClassRefType (ClassType pIdent_)) = foldl (\a (i,_) -> toCompInter i ++ a) [] pIdent_
        implComp' (ArrayType _) = []
      
toCompInter :: Ident -> [CompInter]
toCompInter (Ident "Comparator") = [Comparator]
toCompInter (Ident "Comparable") = [Comparable]
toCompInter _ = []

toComparator :: Ident -> Maybe CompInter
toComparator (Ident "compare")   = Just Comparator
toComparator (Ident "compareTo") = Just Comparable
toComparator _ = Nothing
 
-- The hasComp parameter specifies whether this class implements the 
-- one of the interfaces
-- It does not handle local classes definitions yet.
getComparators :: [CompInter] -> Decl -> [MemberDecl]
getComparators hasComp (MemberDecl mdecl) = getComparator hasComp mdecl
getComparators hasComp (InitDecl _ _) = []

getComparator :: [CompInter] -> MemberDecl -> [MemberDecl]
getComparator hasComp decl = case decl of
    FieldDecl mods ty vardecls -> []    
    MethodDecl mods typars mTy ident pars exTy mBody -> 
      case toComparator ident of
          Nothing -> []
          Just comp -> if comp `elem` hasComp
                       then [decl]
                       else []
    ConstructorDecl mods typars ident pars exTy cBody -> []
    MemberClassDecl clDecl -> [] -- getComps clDecl
    MemberInterfaceDecl iDecl -> []

-- Normalization procedure
--  Receives a comparator and normalize the code to not contain:
--   Loops
--    ...

-- k-Safety hyperproperty
--type HyperProp = (Precondition, Postcondition)
--type Precondition = []
-- This spec specifies how many runs we are interested 
type Spec = Int
    
--  type Substitutions = Map Ident Ident
        
getObjectType :: Comparator -> String
getObjectType (Comp (Ident str) _ ) = str

getParameters :: Comparator -> [FormalParam]
getParameters (Comp _ (MethodDecl _ _ _ _ pars _ _)) = pars
getParameters _ = error "getParameters"

getParIdents :: [FormalParam] -> [String]
getParIdents pars = map getParIdent pars

getParIdent :: FormalParam -> String
getParIdent (FormalParam _ _ _ (VarId ident)) = toString ident
getParIdent _ = error "getParIdent"

toString :: Ident -> String
toString (Ident str) = str

getBlocks :: [Comparator] -> [Block]
getBlocks comps = map getBlock comps
    where getBlock (Comp _ (MethodDecl _ _ _ _ _ _ (MethodBody Nothing))) = error "getBlock"
          getBlock (Comp _ (MethodDecl _ _ _ _ _ _ (MethodBody (Just b)))) = b

{-
Alfa-renaming:
  - I need to substitute the object parameters
  - I need to substitute the local variables with some subscript 
-}

-- alfa: performs the renaming
--  - the integer is the subscript
--  - assumes that the comparator is already normalized
class Renamable a where
    rename :: Int -> a -> a

instance Renamable Comparator where
    rename idx (Comp mth mdecl) = Comp mth $ rename idx mdecl
    
instance Renamable MemberDecl where
    rename idx mdecl = case mdecl of 
        MethodDecl mods typar mty ident pars exty body -> 
            let npars = map (rename idx) pars
                nbody = rename idx body
            in MethodDecl mods typar mty ident npars exty nbody
        _ -> error "attemping to rename a MemberDecl which is not a MethodDecl"

instance Renamable FormalParam where
    rename idx (FormalParam mods ty arity varid) = FormalParam mods ty arity $ rename idx varid

instance Renamable VarDecl where
    rename idx (VarDecl vardeclid minit) = VarDecl (rename idx vardeclid) minit

instance Renamable VarDeclId where
    rename idx (VarId ident) = VarId $ rename idx ident
    rename idx (VarDeclArray varid) = VarDeclArray $ rename idx varid

instance Renamable Ident where
    rename idx (Ident str) = Ident $ str ++ show idx

instance Renamable (Maybe Ident) where
    rename idx Nothing = Nothing
    rename idx (Just ident) = Just $ rename idx ident
    
instance Renamable MethodBody where
    rename idx (MethodBody mbody) = case mbody of
        Nothing -> MethodBody Nothing
        Just block -> MethodBody $ Just $ rename idx block

instance Renamable Block where
    rename idx (Block block) = Block $ map (rename idx) block
    
instance Renamable BlockStmt where
    rename idx stm = case stm of
        BlockStmt st -> BlockStmt $ rename idx st
        LocalVars mods ty vardecls -> LocalVars mods ty $ map (rename idx) vardecls
        LocalClass classdecl -> error "rename: LocalClass is not supported"

instance Renamable Stmt where
    rename idx stm = case stm of 
        StmtBlock block -> StmtBlock $ rename idx block
        IfThen cond _then -> IfThen (rename idx cond) $ rename idx _then 
        IfThenElse cond _then _else -> IfThenElse (rename idx cond) (rename idx _then) (rename idx _else)
        While cond _body -> While (rename idx cond) (rename idx _body)
        BasicFor mForInit mExp mLExp _body -> error "rename: BasicFor not supported"
        EnhancedFor mods ty ident expr _body -> error "rename: EnhancedFor not supported"
        Empty -> Empty
        ExpStmt expr -> ExpStmt $ rename idx expr
        Assert expr mexpr -> Assert (rename idx expr) (rename idx mexpr)
        Switch expr lSwitchBlock -> error "rename: Switch not supported"
        Do _body cond -> error "rename: Do not supported"
        Break mident -> Break $ rename idx mident
        Continue mident -> Continue $ rename idx mident
        Return mexp -> Return $ rename idx mexp
        Synchronized _ _ -> error "rename: Synchronized not supported"
        Throw expr -> Throw $ rename idx expr
        Try block lCatch finally -> error "rename: Try not supported"
        Labeled ident _stmt -> Labeled (rename idx ident) $ rename idx _stmt

instance Renamable (Maybe Exp) where
    rename idx Nothing = Nothing
    rename idx (Just expr) = Just $ rename idx expr
    
instance Renamable Exp where
    rename idx _exp = case _exp of 
        Lit lit -> Lit lit
        ClassLit mty -> ClassLit mty
        This -> This
        ThisClass name -> ThisClass name
        InstanceCreation lTyArgs classTy args mBody -> error "rename: InstanceCreation not supported"
        QualInstanceCreation expr lTyArgs ident args mBody -> error "rename: QualInstanceCreation not supported"
        ArrayCreate ty exprs int -> error "rename: ArrayCreate not supported" 
        ArrayCreateInit ty int aInit -> error "rename: ArrayCreateInit not supported" 
        ArrayAccess aIndex -> error "rename: ArrayAccess not supported"
        FieldAccess fieldAccess -> FieldAccess $ rename idx fieldAccess
        MethodInv methodInvocation -> MethodInv $ rename idx methodInvocation
        ExpName name -> ExpName $ rename idx name
        PostIncrement expr -> PostIncrement $ rename idx expr
        PostDecrement expr -> PostDecrement $ rename idx expr
        PreIncrement expr -> PreIncrement $ rename idx expr
        PreDecrement expr -> PreDecrement $ rename idx expr
        PrePlus expr -> PrePlus $ rename idx expr
        PreMinus expr -> PreMinus $ rename idx expr
        PreBitCompl expr -> PreBitCompl $ rename idx expr
        PreNot expr -> PreNot $ rename idx expr
        Cast ty expr -> Cast ty $ rename idx expr
        BinOp lhs op rhs -> BinOp (rename idx lhs) op (rename idx rhs)
        InstanceOf expr reftype -> error "rename: InstanceOf not supported"
        Cond _cond _then _else -> Cond (rename idx _cond) (rename idx _then) $ rename idx _else
        Assign lhs aOp expr -> Assign (rename idx lhs) aOp $ rename idx expr
            
instance Renamable FieldAccess where
    rename idx fAccess = case fAccess of
        PrimaryFieldAccess obj fIdent -> PrimaryFieldAccess (rename idx obj) fIdent
        SuperFieldAccess fIdent -> SuperFieldAccess fIdent
        ClassFieldAccess cName fIdent -> ClassFieldAccess cName fIdent
    
instance Renamable MethodInvocation where
    rename idx mInv = error "rename: Method Invocation not supported" 
    {-
    case mInv of
        MethodCall name args -> MethodCall name $ map (rename idx) args
        PrimaryMethodCall expr refTys ident args -> PrimaryMethodCall (rename idx ) Exp [RefType] Ident [Argument]
            -- | Invoking a method of the super class, giving arguments for any generic type parameters.
            | SuperMethodCall [RefType] Ident [Argument]
            -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
            | ClassMethodCall Name [RefType] Ident [Argument]
            -- | Invoking a method of a named type, giving arguments for any generic type parameters.
            | TypeMethodCall  Name [RefType] Ident [Argument] 
    -}
    
instance Renamable Name where
    rename idx (Name [ident]) = 
        Name [rename idx ident]
    rename idx (Name idents) = 
        let idents' = map (rename idx) $ init idents
        in Name $ idents' ++ [last idents]
    
instance Renamable Lhs where
    rename idx _lhs = case _lhs of
        NameLhs name -> NameLhs $ rename idx name
        FieldLhs fAccess -> FieldLhs $ rename idx fAccess
        ArrayLhs aIndex -> error "rename: ArrayLhs not supported"