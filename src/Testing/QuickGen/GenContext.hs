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
       , incUses
       , decUses
       ) where

import Testing.QuickGen.Types

import Control.Lens ((^.), (&), (%~), _1, _2, _3)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Reader
import Data.Char
import Data.Maybe
import Language.Haskell.TH (mkName)
import System.Random

type Depth = Int

newtype GenContext a = GC { unGC :: State (Depth, Context, StdGen) a }
  deriving (Functor, Monad)

runGC :: GenContext a -> Int -> Context -> a
runGC (GC g) seed ctx = evalState g (0, ctx, gen)
  where
    gen = snd . next . mkStdGen $ seed

getDepth :: GenContext Depth
getDepth = GC $ fmap (^. _1) get

getRandomR :: (Int, Int) -> GenContext Int
getRandomR p = GC $ state f
  where
    f (d, c, g) = let (a, g') = randomR p g in (a, (d, c, g'))

getRandomBool :: GenContext Bool
getRandomBool = getRandomR (0,1) >>= return . (==1)

localLambda :: [Type] -> GenContext a -> GenContext a
localLambda ts (GC g) = GC $ do
    (depth, c, gen) <- get
    let len = length ts
        uses = 10 -- FIXME: arbitrarily chosen
        vars = C [ (Just uses, Prim (toVar i, [], [t]))
                 | (i, t) <- zip [depth..] ts
                 ]
    put (depth + len, vars <> c, gen)
    a <- g
    modify (\(_, C c, gen) -> (depth, C (drop len c), gen))
    return a
  where
    -- FIXME: might capture variable names
    toVar i = VarE (mkName [chr (i + ord 'a')])

getMatching :: Type -> GenContext [Primitive]
getMatching t = GC $ do
    ps <- fmap (unContext . (^. _2)) get
    return [ Prim (e, c, ts')
           | (mn, (Prim (e, c, ts))) <- ps
           , Just ts' <- [ matchWith t (c, ts) ]
           , maybe True (> 0) mn
           ]

modContext :: (Context -> Context) -> GenContext ()
modContext f = GC $ modify (& _2 %~ f)

decUses :: Primitive -> GenContext ()
decUses p = modContext (C . go . unContext)
  where
    go [] = []
    go (np@(mn, p') : ps)
        | p == p'   = case mn of
            Just n  -> if n == 0 then error "wat" else (Just (n-1), p') : ps
            Nothing -> (Nothing, p') : ps
        | otherwise = np : go ps

incUses :: Primitive -> GenContext ()
incUses p = modContext (C . go . unContext)  where
    go [] = []
    go (np@(mn, p') : ps)
        | p == p'   = (fmap (+1) mn, p') : ps
        | otherwise = np : go ps

-- TODO: real matching of types
matches :: Cxt -> Type -> Type -> Bool
matches _ = (==)
