{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Testing.QuickGen.Types
       ( Exp(..)
       , Type(..)
       , Cxt(..)
       , HValue(..)
       , Context(..)
       , Primitive(..)
--        , toContext
       , consContext

       -- rexports
       , (<>)
       ) where

import Control.Monad
import Data.List (lookup)
import Data.Maybe (fromJust, maybe, listToMaybe)
import Data.Monoid
import Language.Haskell.TH
import System.Random

type Uses = Int

newtype Primitive = Prim { unPrimitive :: (HValue, Exp, Type) }

newtype Context = C { unContext :: [(Int, Primitive)] }

consContext :: Uses -> Primitive -> Context -> Context
consContext uses p (C ctx) = C $ (uses, p) : ctx

instance Monoid Context where
    mempty  = C []
    mappend (C c1) (C c2) = C $ mappend c1 c2

instance Show Primitive where
    show (Prim (_, e, t)) = "Prim " ++ show e ++ " :: " ++ show t

instance Show Context where
    show = show . map showPrim . unContext
      where
        showPrim (uses, p) = "Uses: " ++ show uses ++ " " ++ show p

-- TODO: figure out why this is needed
newtype HValue = HV (forall a. a)
