{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Testing.QuickGen.GenContext
       ( GenContext
       , Depth
       , getDepth
       , getRandomR
       , getRandomBool
       , getMatching
       -- , getId
       , runGC
       , localLambda
       ) where

import Testing.QuickGen.Types

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Reader
import Data.Char
import Data.Maybe
import Language.Haskell.TH (mkName)
import System.Random

type Depth = Int

newtype GenContext a = GC { unGC :: ReaderT (Depth, Context) (State StdGen) a }
  deriving (Functor, Monad)

runGC :: GenContext a -> Int -> Context -> a
runGC g seed ctx = evalState (runReaderT (unGC g) (0, ctx)) gen
  where
    gen = snd . next . mkStdGen $ seed

getDepth :: GenContext Depth
getDepth = GC $ fmap fst ask

getRandomR :: (Int, Int) -> GenContext Int
getRandomR p = GC $ state (randomR p)

getRandomBool :: GenContext Bool
getRandomBool = getRandomR (0,1) >>= return . (==1)

localLambda :: [Type] -> GenContext a -> GenContext a
localLambda ts g = GC $ do
    (depth, ctx) <- ask
    let len  = length ts
        uses = 10 -- FIXME: arbitrarily chosen
        vars = C [ (uses, Prim (toVar i, [], [t]))
                 | (i,t) <- zip [depth..] ts
                 ]
    local (\(m, c) -> (m+len, vars <> c)) (unGC g)
  where
    -- TODO: check capture of var
    toVar i = VarE (mkName [chr (i + ord 'a')])

getMatching :: Type -> GenContext [Primitive]
getMatching t = GC $ do
    ps <- fmap (unContext . snd) ask
    return [ p
           | (_, p@(Prim (_, c, (t':_)))) <- ps
           , matches c t t'
           ]
    -- undefined

-- TODO: real matching of types
matches :: Cxt -> Type -> Type -> Bool
matches _ = (==)
