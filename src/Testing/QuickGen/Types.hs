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
       , Language
       , Seed
       ) where

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Language.Haskell.TH as TH

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
-- > FunT [ConT "Maybe" ["a"], VarT "a"]
data SType =
    FunT [SType]
  | VarT Name
  | ConT Name [Name]

-- | A predicate is a type class constraint, for instance:
--
-- > Eq (Int, a)
--
-- Will be represented as:
--
-- > ClassP "Eq" (ConT "Tup2" [ConT "Int" [], VarT "a"])
data Pred = ClassP Name SType

-- | The constraints is a, possibly empty, list of predicates.
type Cxt = [Pred]

-- | A type is a simple type with a list of variable names used in the
-- type and possibly constraints for the names.
data Type = ForallT [Name] Cxt SType

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

-- | A mapping from unique `Name's to `Type's.
type Substitution = Map Name Type

-- | The current lambda depth when generating expressions. This is
-- used to select the next variable names when generating lambda
-- abstractions.
type Depth = Nat

-- | A mapping from a type class name to a list of super classes and
-- all template haskell instance declarations for the type class.
type ClassEnv = Map Name ([Name], [TH.InstanceDec])

-- | The representation of a user defined \"language\" containing all
-- `Constructor' that may be used when generating expressions. The
-- `ClassEnv' contains all relevant classes needed to do constraint
-- solving for the types mentioned in any of the `Constructor's.
newtype Language = L (ClassEnv, [Constructor])

-- | A random seed.
type Seed = Int
