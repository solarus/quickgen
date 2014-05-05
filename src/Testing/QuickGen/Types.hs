{-# LANGUAGE TemplateHaskell
           , TupleSections #-}

module Testing.QuickGen.Types
       ( Nat
       , Name
       , SType(..)
       , Pred(..)
       , Cxt
       , Quantifier(..)
       , Variable
       , Type(..)
       , Constructor
       , Exp(..)
       , Id
       , Uses
       , Context
       , Substitution
       , Substitutable
       , Depth
       , ClassEnv
       , Language(..)
       , Seed

       , thTypeToType
       , thCxtToCxt
       , getClassNames
       , getCxtNames

       -- Type/SType functions
       , getVars
       , bindForall
       , forallToUndecided
       , isSimple

       -- ClassEnv functions
       , emptyEnv
       , insertEnv
       , lookupEnv

       -- Substitution functions
       , emptySubst
       , singletonSubst
       , (|->)
       , lookupSubst
       , insertSubst
       , differenceSubst
       , toSubst
       , unionSubst
       , unionsSubst
       , sizeSubst
       , apply

       -- Context functions
       , foldrContext
       , filterContextByType

       -- Name functions
       , TH.mkName
       , appendName
       ) where

import           Control.Monad (foldM)
import           Data.Char
import           Data.List (intercalate, isInfixOf, nub, partition)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Language.Haskell.TH.Syntax as TH

import           Testing.QuickGen.THInstances ()

--------------------------------------------------
-- Types

-- | Natural numbers
type Nat = Int

-- | Names are template haskell names
type Name = TH.Name

data Quantifier = Forall | Undecided
  deriving (Eq, Show)

instance TH.Lift Quantifier where
    lift Forall    = [| Forall |]
    lift Undecided = [| Undecided |]

type Variable = (Nat, Quantifier)

-- | Functions of simple types and type constructors with n type
-- arguments. In the `FunT' case the argument types will be reversed
-- i.e. the type:
--
-- > a -> b
--
-- will be represented as:
--
-- > [VarT "b", VarT "a"]
--
-- `FunT' will never be used with less than two arguments. For example
-- the type for the constructor
--
-- > Just :: a -> Maybe a
--
-- will be represented as:
--
-- > FunT [ConT "Maybe" [VarT "a"], VarT "a"]
data SType =
    FunT [SType]
  | VarT Variable
  | ConT Name [SType]
  | ListT SType
  deriving (Eq)

instance Show SType where
    show (FunT ts)   = intercalate " -> " (map (paren . show) (reverse ts))
      where paren s = if ' ' `elem` s then "(" ++ s ++ ")" else s
    show (VarT (n, Forall))    = showForall n
    show (VarT (n, Undecided)) = showUndecided n
    show (ConT n [])
        | all isDigit n' = showTVar (read n')
        | otherwise      = n'
      where
        n' = show n


    show (ConT n ts) = show n ++ " " ++ unwords (map show ts)
    show (ListT t)   = "[" ++ show t ++ "]"

instance TH.Lift SType where
    lift (FunT ts)   = [| FunT ts |]
    lift (VarT (n, q))  = [| VarT (n, q) |]
    lift (ConT n ns) = [| ConT n ns |]
    lift (ListT n)   = [| ListT n |]

-- | A predicate is a type class constraint, for instance:
--
-- > Eq (Int, a)
--
-- Will be represented as:
--
-- > ClassP "Eq" [ConT "Tup2" [ConT "Int" [], VarT "a"]]
data Pred = ClassP Name [SType]
  deriving (Eq)

instance Show Pred where
    show (ClassP n ts) = show n ++ " " ++ unwords (map show ts)

instance TH.Lift Pred where
    lift (ClassP n st) = [| ClassP n st |]

-- | The constraints is a, possibly empty, list of predicates.
type Cxt = [Pred]

-- | A type is a simple type with a list of quantified variable names
-- used in the type and possibly constraints for the names.
data Type = Type [Variable] Cxt SType
  deriving (Eq)

instance Show Type where
    show (Type vs cxt st) = fs' ++ es' ++ cxt' ++ show st
      where
        (fs, es) = partition ((Forall ==) . snd) vs
        showQuant _ _ [] = ""
        showQuant s f xs = s ++ " " ++ unwords (map (f . fst) xs) ++ ". "
        fs' = showQuant "Forall" showForall fs
        es' = showQuant "Undecided" showUndecided es
        cxt' = case cxt of
            []  -> ""
            [c] -> show c ++ " => "
            _   -> "(" ++ intercalate ", " (map show cxt) ++ ")" ++ " => "

instance TH.Lift Type where
    lift (Type vs cs st) = [| Type vs cs st |]

-- | A constructor is a name for a constructor (for instance `id' or
-- `Just') together with its, possibly specialized, type.
type Constructor = (Name, Type)

-- | An expression is either the name of a `Constructor', an
-- expression applied to another expression or a lambda expression.
-- The list of `Name's in `LamE' will always be non empty.
data Exp =
    ConE Name
  | AppE Exp Exp
  | LamE [Name] Exp
  deriving (Eq)

instance Show Exp where
    show (ConE n)     = showVar n
    show (AppE e1 e2) = show e1 ++ " " ++ paren (show e2)
      where
        paren e
            | ' ' `elem` e = "(" ++ e ++ ")"
            | otherwise    = e
    show (LamE ns e) = "\\" ++ unwords (map showVar ns) ++ " -> " ++ show e

showVar :: Name -> String
showVar v
    | "_lam_" `isInfixOf` v' = drop 5 v'
    | otherwise              = v'
  where v' = show v

-- | Represents an unique id for a constructor.
type Id = Nat

-- | Represents the number of uses left for a constructor. `Nothing'
-- represents an unlimited number of uses left.
type Uses = Maybe Nat

-- | A mapping from `Id's to constructors and their number of uses.
type Context = Map Id (Uses, Constructor)

-- | A mapping from unique `Nat's to `SType's.
type Substitution = Map Nat ([Variable], SType)

-- | The current lambda depth when generating expressions. This is
-- used to select the next variable names when generating lambda
-- abstractions.
type Depth = Nat

-- | A mapping from a type class name to a list of super classes and
-- all template haskell instance declarations for the type class.
type ClassEnv = Map Name ([Name], [TH.InstanceDec])

-- | The representation of a user defined \"language\" containing all
-- `Constructor's that may be used when generating expressions. The
-- `ClassEnv' contains all relevant classes needed to do constraint
-- solving for the types mentioned in any of the `Constructor's.
data Language = L ClassEnv [Constructor]
  deriving (Eq, Show)

instance TH.Lift Language where
    lift (L env cs) = [| L $(liftMap env) cs |]
      where
        liftMap :: Map Name ([Name], [TH.InstanceDec]) -> TH.Q TH.Exp
        liftMap m = do
            TH.VarE fromList <- [| M.fromList |]
            let elems = M.assocs m
            return . TH.AppE (TH.VarE fromList) =<< [| elems |]

-- | A random seed.
type Seed = Int

-- | Converts a Template Haskell type into the representation used by
-- this library. Currently does not support rank > 1 types.
-- thTypeToType :: Nat -> TH.Type -> (Nat, Type)
thTypeToType :: TH.Type -> Type
thTypeToType (TH.ForallT bs cs t) = Type vs cs' t'
  where
    bs'   = map parseBinder bs
    cs'   = thCxtToCxt [] cs
    t'    = thTypeToSType subst t
    subst = zip bs' [0..]
    vs    = map ((,Forall) . snd) subst

    parseBinder (TH.PlainTV m) = m
    parseBinder b = error $ "thTypeToType: Binder not matched " ++ show b
thTypeToType t = Type [] [] (thTypeToSType [] t)

thCxtToCxt :: [(Name, Int)] -> TH.Cxt -> Cxt
thCxtToCxt env cs = map f cs
  where
    f (TH.ClassP n ts) = ClassP n (map (thTypeToSType env) ts)
    f c = error $ "thTypeToType: Constraint not matched " ++ show c

thTypeToSType :: [(Name, Int)] -> TH.Type -> SType
thTypeToSType env (TH.VarT name) = VarT (fromJust (lookup name env), Forall)
thTypeToSType env t@(TH.AppT (TH.AppT TH.ArrowT _) _) = FunT (go [] t)
  where
    go acc (TH.AppT (TH.AppT TH.ArrowT t') rest) = go (thTypeToSType env t' : acc) rest
    go acc a = thTypeToSType env a : acc
-- At this point it has to be a ConT or ListT applied to some arguments
thTypeToSType env t = go [] t
  where
    go args (TH.ConT name) = ConT name args
    go [t'] TH.ListT       = ListT t'
    go args (TH.AppT a b)  = go (thTypeToSType env b : args) a
thTypeToSType _ t = error $ "thTypeToSType: Type not matched " ++ show t

-- | Gets all class names mentioned in a `Type'.
getClassNames :: Type -> [Name]
getClassNames (Type _ cxt _) = getCxtNames cxt

getCxtNames :: Cxt -> [Name]
getCxtNames cxt = nub [ n | ClassP n _ <- cxt ]


--------------------------------------------------
-- ClassEnv functions

emptyEnv :: ClassEnv
emptyEnv = M.empty

insertEnv :: Name -> ([Name], [TH.InstanceDec]) -> ClassEnv -> ClassEnv
insertEnv = M.insert

lookupEnv :: Name -> ClassEnv -> Maybe ([Name], [TH.InstanceDec])
lookupEnv = M.lookup


--------------------------------------------------
-- Substitution functions

emptySubst :: Substitution
emptySubst = M.empty

singletonSubst :: Nat -> SType -> Substitution
singletonSubst n st = M.singleton n (getVars st, st)

(|->) :: Nat -> SType -> Substitution
(|->) = singletonSubst

lookupSubst :: Nat -> Substitution -> Maybe ([Variable], SType)
lookupSubst = M.lookup

insertSubst :: Nat -> ([Variable], SType) -> Substitution -> Substitution
insertSubst = M.insert

differenceSubst :: Substitution -> Substitution -> Substitution
differenceSubst = M.difference

toSubst :: [(Nat, ([Variable], SType))] -> Substitution
toSubst = M.fromList

unionSubst :: Monad m => Substitution -> Substitution -> m Substitution
unionSubst s1 s2 = case M.foldrWithKey f (Just s1) s2 of
    Nothing -> fail "Substitutions doesn't match"
    Just s  -> return s
  where
    f _ _ Nothing  = Nothing
    f k a (Just s) = case lookupSubst k s of
        Just b  -> if a == b then Just s else Nothing
        Nothing -> Just (insertSubst k a s)

unionsSubst :: Monad m => [Substitution] -> m Substitution
unionsSubst ss = foldM unionSubst emptySubst ss

sizeSubst :: Substitution -> Int
sizeSubst = M.size

class Substitutable a where
    apply :: Substitution -> a -> a

instance Substitutable SType where
    apply s (FunT ts)      = FunT (apply s ts)
    apply s t@(VarT (n,_)) = maybe t snd (lookupSubst n s)
    apply s (ConT c ts)    = ConT c (apply s ts)
    apply s (ListT t)      = ListT (apply s t)

instance Substitutable Pred where
    apply s (ClassP n ts) = ClassP n (apply s ts)

instance Substitutable Type where
    apply s (Type _ cxt st) = Type vs' cxt' st'
      where
        vs'  = getVars st'
        cxt' = apply s cxt
        st'  = apply s st

instance Substitutable a => Substitutable [a] where
    apply s = map (apply s)


--------------------------------------------------
-- Type/SType functions

getVars :: SType -> [Variable]
getVars (FunT ts)   = nub (concatMap getVars ts)
getVars (VarT n)    = [n]
getVars (ConT _ ts) = nub (concatMap getVars ts)
getVars (ListT t)   = getVars t

bindForall :: Type -> Type
bindForall (Type vs cxt st) = Type vs' cxt' st'
  where
    (fs, vs') = partition ((== Forall) . snd) vs
    subst     = toSubst [ (vn, ([], ConT (TH.mkName (show vn)) []))
                        | (vn, _) <- fs
                        ]
    cxt'      = apply subst cxt
    st'       = apply subst st

forallToUndecided :: SType -> SType
forallToUndecided   (FunT ts)          = FunT (map forallToUndecided ts)
forallToUndecided   (VarT (n, Forall)) = VarT (n, Undecided)
forallToUndecided t@(VarT _)           = t
forallToUndecided   (ConT n ts)        = ConT n (map forallToUndecided ts)
forallToUndecided   (ListT st)         = ListT (forallToUndecided st)

showTVar :: Nat -> String
showTVar n = let (c, n') = n `divMod` 28
             in (chr (ord 'a' + c)) : "_" ++ show n'

showForall :: Nat -> String
showForall n = "∀_" ++ show n

showUndecided :: Nat -> String
showUndecided n = "∃_" ++ show n

-- | Returns if a type is simple, i.e. a constructor of this type
-- won't introduce any sub goals. Using a constructor with a simple
-- type will always terminate the current subgoal.
isSimple :: Type -> Bool
isSimple (Type _ _ st) = go st
  where
    go (FunT _) = False
    go (VarT (_, Forall)) = True
    go (VarT (_, Undecided)) = False
    go (ListT t) = go t
    go (ConT _ ts) = all go ts


--------------------------------------------------
-- Context functions

foldrContext :: (Id -> (Uses, Constructor) -> a -> a) -> a -> Context -> a
foldrContext = M.foldrWithKey

filterContextByType :: (Type -> Maybe a) -> Context -> [(Id, Constructor, a)]
filterContextByType f = M.foldrWithKey f' []
  where
    f' i (Nothing, c@(_,t)) cs = case f t of
        Just a  -> (i, c, a) : cs
        Nothing -> cs
    f' i (Just u, c@(_,t)) cs
        | u > 0 = case f t of
            Just a  -> (i, c, a) : cs
            Nothing -> cs
        | otherwise = cs


--------------------------------------------------
-- Name functions

appendName :: String -> Name -> Name
-- FIXME: Probably not correct (look at TH.nameBase, TH.nameModule)
-- but works for now.
appendName s n = TH.mkName (show n ++ s)
