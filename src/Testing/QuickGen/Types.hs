{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Testing.QuickGen.Types
       ( Exp(..)
       , Pat(..)
       , Type(..)
       , Cxt(..)
       , HValue(..)
       , Context(..)
       , Uses
       , Primitive(..)
       , ClassEnv
       , mkName
       , consContext
       , listToContext
       , extractPrimType
       -- rexports
       , (<>)
       , pprint
       ) where

import Control.Lens ((&), _2, (%~), _1, (.~))
import Control.Monad
import Data.List (lookup)
import Data.Maybe (fromJust, maybe, listToMaybe)
import Data.Monoid
import Language.Haskell.TH
import System.Random

type Uses = Maybe Int

-- | A primitive value is an Expression together with the type of the
--   expression. The type is represented as a list where the first
--   element is the return type, the second element is the last
--   argument. The type `(a -> b) -> a -> b' whould then be
--   represented as `[b, a, (a -> b)]'.
newtype Primitive = Prim { unPrimitive :: (Exp, Cxt, [Type]) }

instance Eq Primitive where
    Prim (e1, _, _) == Prim (e2, _, _) = e1 == e2

newtype Context = C { unContext :: [(Uses, Primitive)] }

consContext :: Uses -> Primitive -> Context -> Context
consContext uses p (C ctx) = C $ (uses, p) : ctx

listToContext :: Int -> [(Exp, Type)] -> Context
listToContext uses xs = C [ ctxFun x | x <- xs ]
  where
    ctxFun (e, t) = let (c, t') = extractPrimType t
                        uses' = case t' of
                            [_] -> Nothing
                            _   -> Just uses
                    in (uses', Prim (e, c, t'))

instance Monoid Context where
    mempty  = C []
    mappend (C c1) (C c2) = C $ mappend c1 c2

instance Show Primitive where
    show (Prim (e, _, t)) = "Prim " ++ show e ++ " :: " ++ show t

instance Show Context where
    show = show . map showPrim . unContext
      where
        showPrim (uses, p) = "Uses: " ++ show uses ++ " " ++ show p

-- TODO: figure out why this is needed
newtype HValue = HV (forall a. a)

type ClassEnv = [(Name, [InstanceDec])]

extractPrimType :: Type -> (Cxt, [Type])
extractPrimType t = (cxt, reverse ts)
  where
    (cxt, ts) = go t
    go (AppT (AppT ArrowT t1) rest) = go rest & _2 %~ (t1:)

    -- TODO: merge constraints somehow. Right now I'm only overriding the
    -- current value, i.e. I'm hoping there were no constraints before.
    go (ForallT vars cxt rest)      = go rest & _1 .~ cxt

    go a                            = ([], [a])
