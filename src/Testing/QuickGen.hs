{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Testing.QuickGen where

import Control.Lens (_1, _2, _3, (^.), to, (.~), (&), (%~))
import Data.Char (ord, chr)
import Data.Maybe (catMaybes, listToMaybe, fromJust)
import Data.Typeable
import Language.Haskell.TH (mkName)
import System.Random
import Test.Feat

-- Local imports
import Testing.QuickGen.GenContext
import Testing.QuickGen.TH
import Testing.QuickGen.Types

argsAndRet :: Type -> (Cxt, [Type])
argsAndRet (AppT (AppT ArrowT t1) rest) = argsAndRet rest & _2 %~ (t1:)

-- TODO: merge constraints somehow. Right now I'm only overriding the
-- current value, i.e. I'm hoping there were no constraints before.
argsAndRet (ForallT vars cxt rest)      = argsAndRet rest & _1 .~ cxt

argsAndRet a                            = ([], [a])

generate :: [Type] -> GenContext Exp
generate = go . reverse
  where
    go :: [Type] -> GenContext Exp
    go (match : args) = localLambda args $ do
        fmap fromJust (randomMatching match)

randomMatching :: Type -> GenContext (Maybe Exp)
randomMatching match = undefined

-- | First argument is type to match. Second is a list of current
-- argument types (innermost binding first).
-- randomMatching' :: Type -> [Type] -> GenContext (Maybe MatchData)
-- randomMatching' match args = do
--     let p = randBool >>= \b -> if b then randomLambda else randomPrimitive

--     -- Arbitrarily retry 10 times
--     replicateM 10 p >>= return . listToMaybe . catMaybes
--   where
--     mRandElem xs
--         | len == 0  = return Nothing
--         | otherwise = randR (0, len-1) >>= return . Just . (xs!!)
--       where len = length xs

--     randomLambda  = do
--         let mVars = [ MatchLambda (chr (i + ord 'a'))
--                     | (i, t) <- zip [0..] (reverse args)
--                     -- TODO: match polymorphic constants
--                     , t == match
--                     ]
--         mRandElem mVars

--     randomPrimitive = do
--         cs <- fmap unContext getContext
--         let mPrims = [ MatchPrim id | (id, (_, Prim (_, _, t))) <- cs, match == t ]
--         mRandElem mPrims

--------------------------------------------------
-- Testing

-- bar = 5 :: Int

-- foo = $(p [| ( bar :: Int
--              , map :: (a -> b) -> [a] -> [b]
--              , (+) :: Int -> Int -> Int
--              , id  :: a -> a
--              )
--            |])

-- t = snd (argsAndRet ((foo !! 2) ^. _3))
-- c = toContext 10 foo

-- f n = let (_,g) = next (mkStdGen n)
--           s = newState g c
--       in fst (runGC (generate t) s)

-- g n = let s = newState (mkStdGen n) c
--       in fst (runGC (randR (0,1)) s)

-- h n = fst (randomR (0,2) (mkStdGen n))
