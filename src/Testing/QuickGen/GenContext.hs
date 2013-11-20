{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Testing.QuickGen.GenContext
       ( GenContext
       , getRandomR
       , getRandomBool
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

type MaxDebruijn = Int

newtype GenContext a = GC { unGC :: ReaderT (MaxDebruijn, Context) (State StdGen) a }
  deriving (Functor, Monad)

runGC :: GenContext a -> Int -> Context -> a
runGC g seed ctx = evalState (runReaderT (unGC g) (0, ctx)) gen
  where
    gen = snd . next . mkStdGen $ seed

getRandomR :: (Int, Int) -> GenContext Int
getRandomR p = GC $ state (randomR p)

getRandomBool :: GenContext Bool
getRandomBool = getRandomR (0,1) >>= return . (==1)

localLambda :: [Type] -> GenContext a -> GenContext a
localLambda ts g = GC $ do
    (maxDebruijn, ctx) <- ask
    let len  = length ts
        uses = 10 -- FIXME: arbitrarily chosen
        vars = C [ (uses, Prim (error "GenContext.localLambdaVarE: wat", toVar i, t))
                 | (i,t) <- zip [maxDebruijn+1..] ts
                 ]
    local (\(m, c) -> (m+len, vars <> c)) (unGC g)
  where
    -- TODO: check capture of var
    toVar i = VarE (mkName [chr (i + ord 'a')])

-- TODO: Make this function total?
-- getId :: Id -> GenContext Primitive
-- getId id = GC $ fmap (snd . fromJust . lookup id . unContext . snd) ask
