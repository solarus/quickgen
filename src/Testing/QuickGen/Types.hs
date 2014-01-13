{-# LANGUAGE TemplateHaskell #-}

module Testing.QuickGen.Types
       ( Nat
       , Name
       , SType(..)
       , Pred(..)
       , Cxt
       , Type(..)
       , Constructor
       , Exp(..)
       , Id
       , Uses
       , Context
       , Substitution
       , Depth
       , ClassEnv
       , Language(..)
       , Seed

       , thTypeToType
       , thCxtToCxt
       , getClassNames
       , getCxtNames
       , TH.mkName

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
       , unionSubst
       , unionsSubst
       ) where

import           Control.Monad (foldM)
import           Data.List (nub)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Language.Haskell.TH.Syntax as TH

import           Testing.QuickGen.THInstances ()

--------------------------------------------------
-- Types

-- | Natural numbers
type Nat = Int

-- | Names are template haskell names
type Name = TH.Name

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
  | VarT Name
  | ConT Name [SType]
  | ListT SType
  deriving (Eq, Show)

instance TH.Lift SType where
    lift (FunT ts)   = [| FunT ts |]
    lift (VarT n)    = [| VarT n |]
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
  deriving (Eq, Show)

instance TH.Lift Pred where
    lift (ClassP n st) = [| ClassP n st |]

-- | The constraints is a, possibly empty, list of predicates.
type Cxt = [Pred]

-- | A type is a simple type with a list of variable names used in the
-- type and possibly constraints for the names.
data Type =
    ForallT [Name] Cxt SType
  | ExistsT [Name] Cxt SType
  deriving (Eq, Show)

instance TH.Lift Type where
    lift (ForallT ns cs st) = [| ForallT ns cs st |]
    lift (ExistsT ns cs st) = [| ExistsT ns cs st |]

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

-- | Represents an unique id for a constructor.
type Id = Nat

-- | Represents the number of uses left for a constructor. `Nothing'
-- represents an unlimited number of uses left.
type Uses = Maybe Nat

-- | A mapping from `Id's to constructors and their number of uses.
type Context = Map Id (Uses, Constructor)

-- | A mapping from unique `Name's to `SType's.
type Substitution = Map Name SType

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
thTypeToType :: TH.Type -> Type
thTypeToType (TH.ForallT bs cs t) = ForallT bs' cs' t'
  where
    bs' = map parseBinder bs
    cs' = thCxtToCxt cs
    t'  = thTypeToSType t

    parseBinder (TH.PlainTV n) = n
    parseBinder b = error $ "thTypeToType: Binder not matched " ++ show b
thTypeToType t = ForallT [] [] (thTypeToSType t)

thCxtToCxt :: TH.Cxt -> Cxt
thCxtToCxt cs = map f cs
  where
    f (TH.ClassP n ts) = ClassP n (map thTypeToSType ts)
    f c = error $ "thTypeToType: Constraint not matched " ++ show c

thTypeToSType :: TH.Type -> SType
thTypeToSType (TH.VarT name) = VarT name
thTypeToSType t@(TH.AppT (TH.AppT TH.ArrowT _) _) = FunT (go [] t)
  where
    go acc (TH.AppT (TH.AppT TH.ArrowT t') rest) = go (thTypeToSType t' : acc) rest
    go acc a = thTypeToSType a : acc
-- At this point it has to be a ConT or ListT applied to some arguments
thTypeToSType t = go [] t
  where
    go args (TH.ConT name) = ConT name args
    go [t'] TH.ListT       = ListT t'
    go args (TH.AppT a b)  = go (thTypeToSType b : args) a
thTypeToSType t = error $ "thTypeToSType: Type not matched " ++ show t

-- | Gets all class names mentioned in a `Type'.
getClassNames :: Type -> [Name]
getClassNames (ForallT _ cxt _) = getCxtNames cxt
getClassNames (ExistsT _ cxt _) = getCxtNames cxt

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

singletonSubst :: Name -> SType -> Substitution
singletonSubst = M.singleton

(|->) :: Name -> SType -> Substitution
(|->) = singletonSubst

lookupSubst :: Name -> Substitution -> Maybe SType
lookupSubst = M.lookup

insertSubst :: Name -> SType -> Substitution -> Substitution
insertSubst = M.insert

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
