{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Testing.QuickGen.Types
       ( Exp(..)
       , Type(..)
       , Cxt(..)
       , HValue(..)
       , Context(..)
       , Primitive(..)
       , Id
       , newState
       , toContext
       ) where

import Control.Monad
import Data.List (lookup)
import Data.Maybe (fromJust)
import Language.Haskell.TH
import System.Random

type Id = Int

newtype Primitive = Prim { unPrimitive :: (HValue, Exp, Type) }

newtype Context = C { unContext :: [(Id, (Int, Primitive))] }

toContext :: Int -> [(HValue, Exp, Type)] -> Context
toContext uses prims = C [ (n, (uses, Prim p)) | (n, p) <- zip [0..] prims ]

instance Show Primitive where
    show (Prim (_, e, t)) = "Prim " ++ show e ++ " :: " ++ show t

instance Show Context where
    show = show . map showPrim . unContext
      where
        showPrim (id, (uses, p)) = "Id: #" ++ show id ++ " uses: " ++ show uses ++ " " ++ show p

-- TODO: figure out why this is needed
newtype HValue = HV (forall a. a)

newState :: StdGen -> Context -> State
newState = (,)
