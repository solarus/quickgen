{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Monad.State
import System.Random (StdGen)
import qualified Language.Haskell.TH.Syntax as TH

-- | Natural numbers
type Nat = Int

----------------------------------------------------------------------
-- These are the types that are used in the project.
----------------------------------------------------------------------

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
type Id    = Nat

-- | Represents the number of uses left for a constructor. `Nothing'
-- represents an unlimited number of uses left.
type Uses = Maybe Nat

-- | Contains a list of constructors with its `Id' and number of
-- `Uses'.
type Context = [(Id, Uses, Constructor)]

-- | A mapping from unique `Name's to `Type's.
type Substitution = [(Name, Type)]

-- | The current lambda depth when generating expressions. This is
-- used to select the next variable names when generating lambda
-- abstractions.
type Depth = Nat

-- | The ExpGen monad is a state monad with a `Depth', a non-empty
-- list of `Context's, a `StdGen' and a `Substitution'. The last
-- element in the `Context' list contains the user specified standard
-- library and each new element contains the variables introduced by a
-- lambda abstraction.
--
-- NOTE: The substitution might possibly be removed if the `Depth' and
-- @[`Context']@ is moved to a `Reader' instead.
newtype ExpGen a = EG { unEG :: State (Depth, [Context], StdGen, Substitution) a }

-- | A mapping from a type class name to a list of super classes and
-- all template haskell instance declarations for the type class.
type ClassEnv = [(Name, [Name], [TH.InstanceDec])]

-- | The representation of a user defined \"language\" containing all
-- `Constructor' that may be used when generating expressions. The
-- `ClassEnv' contains all relevant classes needed to do constraint
-- solving for the types mentioned in any of the `Constructor's.
type Language = (ClassEnv, [Constructor])

-- | A random seed.
type Seed = Int


----------------------------------------------------------------------
-- The following functions are the API exposed to the user.
----------------------------------------------------------------------

-- | Given a list of `Name's and @`Maybe' `Type'@ (constructors)
-- returns a definition of the `Language' representing the
-- constructors. If the `Type' for one of the constructors is `Nothing
-- then the type will be queried and the most general type will be
-- used. If the `Type' is specified then this possibly specialized
-- type will be used instead. For instance the following type is an
-- example of a specialized type for the constructor `map':
--
-- > (a -> Int) -> [a] -> [Int]
defineLanguage :: [(Name, Maybe Type)] -> TH.Q Language
defineLanguage = undefined

-- | Instance to lift values from `TH.Q Language' to `TH.Q TH.Exp' so
-- that they can be easily spliced using template haskell.
instance TH.Lift Language

-- | Tries to generate a random expression of the given `Type' using
-- the `Constructor's specified in the `Language' definition.
generate :: Type -> Seed -> Language -> Maybe Exp
-- This function should basically be a wrapper around generate'
generate = undefined


----------------------------------------------------------------------
-- This is some of the functions used when generating exressions.
----------------------------------------------------------------------

-- | Given a `Type' returns a randomly selected `Constructor' found in
-- any of the current contexts. The type variables in the returned
-- `Type' only contains freshly generated type variable names that is
-- guaranteed to not be captured by subsequent calls to
-- `randomMatching'. Further, the `Type' for this `Constructor' might
-- be specialized to match the current goal type. I.e. if the current
-- type is `[Int]' and the selected constructor is:
--
-- > ("map", (a -> b) -> [a] -> [b])
--
-- then the returned `Constructor' might be:
--
-- > ("map", (a1 -> Int) -> [a1] -> [Int])
--
-- If no matching constructors can be found then `Nothing' is
-- returned.
--
-- NOTE: Currently the current implementation only does simple type
-- matching to find suitable constructors. Unification might be needed
-- and constraint solving is definitely needed.
--
-- NOTE: By guaranteeing that all type variables are fresh and that
-- they won't be captured by subsequent calls to `randomMatching' then
-- this allows the `Substitition' to be represented as a mapping from
-- unique names to types.
randomMatching :: Type -> ExpGen (Maybe (Id, Constructor))
randomMatching = undefined

-- | Given a type @t@ tries to generate an expression matching @t@. If
-- successful returns an expression and a list of all `Id's for the
-- constructors used to generate the expression.
--
-- There are two cases to consider:
--
-- @t == `ForallT' ns cxt (`FunT' ts)@
--
-- - Here we are generating a function. Just add all the argument
--   types to the `Context' (there is at least one!) and do a
--   recursive call with the head of `ts'. Return the body of the
--   recursive call inside a lambda with the arguments.
--
-- @t == `VarT' name || t == `ConT' name ns@
--
-- - In my current implementation the algorithm that I want to do is
--   something as follows:
--
--   Retry the following @n@ times for some natural number @n@
--   (restart if `Nothing' is returned). Currently @n@ is 10.
--
--   1. Find a random matching constructor @m@ with id @i@ for the
--      type.
--
--   2. Case analysis based on the structure of @m@:
--
--      - If no matching constructor was found then return `Nothing'.
--
--      - If @m@ is a constructor with a non function type either
--        @`VarT' name@ or @`ConT' name _@ then decrease the number of
--        available uses for the constructor and return @([i], `ConE'
--        name)@
--
--      - This is where I'm working the most currently, when @m@ is a
--        function type. The current implementation tries to generate
--        all expressions for the argument types by recursive calls to
--        `generate''. If any of the calls fail concatenate all the
--        @[`Id']@ that has been returned so far and increase the
--        number of available uses for each element in the
--        concatenated list. I think this is correct only when the
--        argument types contains simple types without polymorphic
--        variables (possibly not even then). here's one example that
--        describes the problem:
--
--        - I want to generate an Int and `randomMatching' returns the
--          constructor @(($), (a -> Int) -> a -> Int)@. If I use
--          `generate'' to generate the second argument with type @a@
--          I must return a polymorphic value of type @a@. However in
--          this case since I generated the call to ($) I may
--          specialize @a@ to any type that I want since this has no
--          effect on the return type of this call to @($)@: `Int'.
--          For instance if @i :: Int@ and @d :: Double@ is in scope I
--          might want to generate @($) (\_ -> i) d@ thereby
--          specializing @a@ to `Double'.
--
--        To solve this I can possibly store some information in the state
--        telling which variables may be specialized. If they can be
--        specialized the specialization can be stored in the
--        `Substitution' list.
--
--        Also I think that the above example is equivalent to
--        generating an expression for @exists [a]. (a -> Int) -> a ->
--        Int@ instead of @forall [a]...@. It might be nice to add an
--        existensional type to the `Type' datatype, i.e. @ExistsT ns
--        cxt@.
generate' :: Type -> ExpGen (Maybe ([Id], Exp))
generate' t@(ForallT ns cxt (FunT ts)) = undefined
generate' t@(ForallT ns cxt st)        = undefined

main = putStrLn "Hello world!"
