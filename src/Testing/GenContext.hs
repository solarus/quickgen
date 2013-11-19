{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Testing.QuickGen.GenContext where

import Testing.QuickGen.Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import System.Random

-- type GenState = (StdGen, Context)

newtype GenContext a = GC { unGC :: ReaderT Context (State StdGen) a }
  deriving (Functor, Monad)

instance Functor GenContext where
    fmap = liftM
    -- fmap f (GC g) = GC $ \s -> let (a, s') = g s in (f a, s')

instance Monad GenContext where
    return a = GC (\s -> (a, s))
    GC f >>= g = GC $ \s -> let (a, s')  = f s
                                GC f'    = g a
                            in f' s'

randR :: (Int, Int) -> GenContext Int
randR range = GC $ \(g, s) -> let (i, g') = randomR range g
                              in (i, (g', s))

getContext :: GenContext Context
getContext = GC $ \s@(_, c) -> (c, s)

getId :: Id -> GenContext Primitive
getId id = fmap (snd . fromJust . lookup id . unContext) getContext
