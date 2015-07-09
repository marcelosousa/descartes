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
import Language.Java.Pretty

import Analysis.Types

import Debug.Trace
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
    MethodDecl mods ty rty ident pars exTy (MethodBody Nothing) -> [mdecl]
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
    rename idx (VarDecl vardeclid Nothing) = VarDecl (rename idx vardeclid) Nothing
    rename idx (VarDecl vardeclid (Just i)) = VarDecl (rename idx vardeclid) (Just $ rename idx i)
    
instance Renamable VarInit where
    rename idx (InitExp expr) = InitExp $ rename idx expr
    rename idx (InitArray _) = error "InitArray not supported"
    
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
        BasicFor mForInit mExp mLExp _body -> 
            case mForInit of
                Just (ForLocalVars mods ty vardecls) -> 
                    let b1 = LocalVars mods ty vardecls
                        cond = case mExp of
                            Nothing -> error $ "rename: BasicFor not supported" -- ++ show (mForInit, mExp, mLExp)
                            Just c -> c
                        rest = case mLExp of
                            Nothing -> []
                            Just exps -> map (BlockStmt . ExpStmt) exps
                        body = StmtBlock (Block ((BlockStmt _body):rest))
                        l = BlockStmt $ While cond body
                    in rename idx $ StmtBlock (Block [b1,l])
                Just _ -> undefined
                Nothing -> error $ "rename: BasicFor not supported" -- ++ show (mForInit, mExp, mLExp)

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
    rename idx mInv = case mInv of
        MethodCall name@(Name [Ident "assume"]) args -> MethodCall name $ map (rename idx) args
        MethodCall name@(Name [Ident "nondet"]) args -> MethodCall name $ map (rename idx) args
        MethodCall name@(Name [Ident "Double",Ident "compare"]) args -> MethodCall name $ map (rename idx) args
        MethodCall name@(Name [Ident "Int",Ident "compare"]) args -> MethodCall name $ map (rename idx) args
        MethodCall name@(Name [Ident "String",Ident "compareIgnoreCase"]) args -> MethodCall name $ map (rename idx) args
        MethodCall name args -> MethodCall (rename idx name) $ map (rename idx) args
        _ -> error $ "MethodInvocation not supported: " ++ show mInv
    {-
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

class Rewritable a where
    rewrite :: a -> a

instance Rewritable Comparator where
    rewrite (Comp mth mdecl) = Comp mth $ rewrite mdecl

instance Rewritable MemberDecl where
    rewrite mdecl = case mdecl of 
        MethodDecl mods typar mty ident pars exty body -> 
            let nbody = rewrite body
            in MethodDecl mods typar mty ident pars exty nbody
        _ -> error "attemping to rewrite a MemberDecl which is not a MethodDecl"
        
instance Rewritable MethodBody where
    rewrite (MethodBody mbody) = case mbody of
        Nothing -> MethodBody Nothing
        Just block -> MethodBody $ Just $ rewrite block

instance Rewritable Block where
    rewrite (Block block) = Block $ map rewrite block
    
instance Rewritable BlockStmt where
    rewrite stm = case stm of
        BlockStmt st -> BlockStmt $ rewrite st
        LocalVars mods ty vardecls -> LocalVars mods ty vardecls
        LocalClass classdecl -> error "rewrite: LocalClass is not supported"

instance Rewritable Stmt where
    rewrite stm = case stm of 
        StmtBlock block -> StmtBlock $ rewrite block
        IfThen cond _then -> IfThen cond $ rewrite _then 
        IfThenElse cond _then _else -> IfThenElse cond (rewrite _then) (rewrite _else)
        While cond _body -> transform $ normalize stm 
        BasicFor mForInit mExp mLExp _body -> error "rewrite: BasicFor not supported"
        EnhancedFor mods ty ident expr _body -> error "rewrite: EnhancedFor not supported"
        Empty -> Empty
        ExpStmt expr -> ExpStmt expr
        Assert expr mexpr -> Assert expr mexpr
        Switch expr lSwitchBlock -> error "rename: Switch not supported"
        Do _body cond -> error "rewrite: Do not supported"
        Break mident -> Break mident
        Continue mident -> Continue mident
        Return mexp -> Return mexp
        Synchronized _ _ -> error "rewrite: Synchronized not supported"
        Throw expr -> Throw expr
        Try block lCatch finally -> error "rewrite: Try not supported"
        Labeled ident _stmt -> Labeled ident $ rewrite _stmt
        Assume expr -> Assume expr

-- apply the transform rules
transform :: Stmt -> Stmt
transform (While cond body) = 
    case body of
        StmtBlock (Block bstm) -> 
            let (_cond, _body, (Block rest)) = trans bstm                
                loop = While _cond (StmtBlock _body)
            in StmtBlock $ Block $ [BlockStmt loop] ++ rest
        _ -> error "transform"

trans :: [BlockStmt] -> (Exp, Block, Block)
trans [BlockStmt (IfThenElse _cond (StmtBlock _then) _else)] = 
    let ass = Assume $ PreNot _cond
        _else' = case _else of
            Break _ -> []
            Return _ -> [BlockStmt _else]
            StmtBlock (Block bstm) -> bstm -- need to change this: remove break statement if exists
        rest = Block $ (BlockStmt ass):_else'
    in (_cond, _then, rest)
trans ((BlockStmt (IfThenElse _cond t@(StmtBlock (Block _then)) _else)):r) = 
    let (c', Block s', Block s'') = trans r
        c'' = BinOp _cond And $ wp t c'
        _then' = Block $ _then ++ s'
        _thenRest = StmtBlock $ Block $ [BlockStmt $ Assume _cond] ++ _then ++ s''
        _else' = case _else of
            Break _ -> []
            Return _ -> [BlockStmt _else]
            StmtBlock (Block bstm) -> bstm --init bstm
        _elseRest = StmtBlock $ Block $ (BlockStmt $ Assume (PreNot _cond)):_else'
        rest = Block $ [BlockStmt $ IfThenElse Nondet _thenRest _elseRest]
    in (c'', _then', rest)


-- simple weakest pre-condition
wp :: Stmt -> Exp -> Exp
wp stm phi = case stm of 
    StmtBlock (Block bstm) -> foldl (\phi' (BlockStmt stm) -> wp stm phi') phi bstm
    ExpStmt expr -> wpExpr expr phi
    _ -> error $ "wp:" ++ show stm ++ " not supported"

wpExpr :: Exp -> Exp -> Exp
wpExpr (Assign (NameLhs name) EqualA rhs) phi = replace name rhs phi
wpExpr expr phi = error $ "wpExpr: " ++ show expr ++ " not supported"

replace :: Name -> Exp -> Exp -> Exp
replace name rhs phi = 
    case phi of 
        Lit lit -> phi
        BinOp left op right -> BinOp (replace name rhs left) op (replace name rhs right)
        ExpName _name -> if name == _name then rhs else phi
        PostDecrement expr -> PostDecrement $ replace name rhs expr
        PostIncrement expr -> PostIncrement $ replace name rhs expr
        PreIncrement  expr -> PreIncrement  $ replace name rhs expr
        PreDecrement  expr -> PreDecrement  $ replace name rhs expr
        PrePlus       expr -> PrePlus       $ replace name rhs expr
        PreMinus      expr -> PreMinus      $ replace name rhs expr
        PreBitCompl   expr -> PreBitCompl   $ replace name rhs expr
        PreNot        expr -> PreNot        $ replace name rhs expr
        MethodInv (MethodCall _name args) -> 
            let args' = map (replace name rhs) args
                nname = if name == _name then error "dont know what to do" else _name
            in MethodInv $ MethodCall nname args'
        _ -> error $ "replace: " ++ show phi ++ " not supported"

normalize :: Stmt -> Stmt
normalize (While cond body) = 
    let (lHead, rest) = loopHead body
        lHead' = map BlockStmt lHead
        _then = StmtBlock (Block lHead')
        _else = Break Nothing
        cb1 = IfThenElse cond _then _else
        cbN = loopConditions rest
        nBody = StmtBlock $ Block $ map BlockStmt (cb1:cbN)
        nCond = Lit $ Boolean True
    in if checkBody nBody 
       then While nCond nBody
       else error "normalize: not the right final body format"
normalize _ = error "normalize: not a while loop"

-- check that loop body is of the format in the paper
checkBody :: Stmt -> Bool
checkBody stm = case stm of 
    StmtBlock (Block bstm) -> all checkCondition bstm
    _ -> error "checkBody"
    
checkCondition :: BlockStmt -> Bool
checkCondition (BlockStmt (IfThenElse _ _ _)) = True
checkCondition _ = False

-- loopHead: under some condition, splits the statement 
--  into two parts: 
--   1. The statements that always execute under this condition
--   2. Either the break/return, or a conditional that contains a break
loopHead :: Stmt -> ([Stmt],[Stmt])
loopHead stm = case stm of 
    StmtBlock (Block bstm) -> splitBody bstm
    Break mident -> ([], [stm])
    Return mexp -> ([], [stm])
    IfThen cond _then -> 
        if containsBreak _then
        then ([], [stm])
        else ([stm],[])
    IfThenElse cond _then _else -> 
        if containsBreak _then || containsBreak _else
        then ([], [stm])
        else ([stm],[])
    _ -> ([stm],[])
    
splitBody :: [BlockStmt] -> ([Stmt], [Stmt])
splitBody [] = ([], [])
splitBody ((BlockStmt stm):rest) = 
    case loopHead stm of
        (left, []) -> 
            let (left', right) = splitBody rest
            in (left ++ left', right)
        (left, right) -> (left, right ++ [StmtBlock (Block rest)])        
splitBody l = error $ "splitBody: BlockStmt is not BlockStmt: " ++ show l

loopConditions :: [Stmt] -> [Stmt]
loopConditions [] = []
loopConditions (s:ss) = 
    case s of
        IfThen cond _then -> 
            if containsBreak _then
            then let stm = StmtBlock $ Block $ map BlockStmt ss
                     (lHead, rest) = loopHead stm
                     nCond = PreNot cond
                     nThen = StmtBlock $ Block $ map BlockStmt lHead
                     nIf = IfThenElse nCond nThen _then
                     others = loopConditions rest
                 in nIf:others
            else error "No break"
        IfThenElse cond _then _else -> error "loopConditions: if then else?"
        _ -> error "loopConditions: not a conditional statement"

containsBreak :: Stmt -> Bool
containsBreak stm = case stm of
    StmtBlock (Block bstm) -> any containsBreak' bstm
    IfThen cond _then -> 
        if containsBreak _then
        then error "containsBreak in nested conditional"
        else False
    IfThenElse cond _then _else -> 
        if containsBreak _then || containsBreak _else
        then error "containsBreak in nested conditional"
        else False
    Break _ -> True
    Return _ -> True
    _ -> False

containsBreak' :: BlockStmt -> Bool
containsBreak' (BlockStmt stm) = containsBreak stm
containsBreak' _ = False

replaceExp :: Ident -> Ident -> Exp -> Exp
replaceExp i j phi = 
  case phi of 
    Lit lit -> phi
    BinOp left op right -> BinOp (replaceExp i j left) op (replaceExp i j right)
    ExpName (Name names) -> ExpName $ Name $ map (\ident -> if ident == i then j else ident) names
    PostDecrement expr -> PostDecrement $ replaceExp i j expr
    PostIncrement expr -> PostIncrement $ replaceExp i j expr
    PreIncrement  expr -> PreIncrement  $ replaceExp i j expr
    PreDecrement  expr -> PreDecrement  $ replaceExp i j expr
    PrePlus       expr -> PrePlus       $ replaceExp i j expr
    PreMinus      expr -> PreMinus      $ replaceExp i j expr
    PreBitCompl   expr -> PreBitCompl   $ replaceExp i j expr
    PreNot        expr -> PreNot        $ replaceExp i j expr
    MethodInv (MethodCall _name args) -> 
        let args' = map (replaceExp i j) args
        in MethodInv $ MethodCall _name args'
    _ -> error $ "replace: " ++ show phi ++ " not supported"

