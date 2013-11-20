{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Testing.QuickGen where

import Control.Lens (_1, _2, _3, (^.), to, (.~), (&), (%~))
import Control.Monad
import Data.Char (ord, chr)
import Data.Maybe (catMaybes, listToMaybe, fromJust)
import Data.Typeable
import System.Random
import Test.Feat

-- Local imports
import Testing.QuickGen.GenContext
import Testing.QuickGen.TH
import Testing.QuickGen.Types

import Debug.Trace

generate :: Type -> Int -> Context -> Maybe Exp
generate t seed ctx = case runGC g seed ctx of
    Nothing             -> Nothing
    Just (Prim (e,_,_)) -> Just e
  where
    (_, ts) = extractPrimType t
    g       = generate' ts

generate' :: [Type] -> GenContext (Maybe Primitive)
generate' [t] = replicateM 10 p >>= return . listToMaybe . catMaybes
  where
    p = do
        m <- randomMatching t
        case m of
            Nothing                    -> return Nothing
            Just (_, Prim (e, c, [t]))  -> return m
            Just (Prim (e, c, all@(t:ts))) -> do
                args <- forM ts (generate' . (:[]))
                case toExpList args of
                    Nothing    -> return Nothing
                    Just args' ->
                        let e' = foldl AppE e args'
                        in return (Just (Prim (e, c, [t])))

    toExpList :: [Maybe Primitive] -> Maybe [Exp]
    toExpList xs = fmap (reverse . map ((^. _1) . unPrimitive)) (sequence xs)

generate' all@(t:ts) = do
    ret <- localLambda ts (generate' [t])
    case ret of
        Nothing        -> return Nothing
        Just (Prim (e, c, _)) -> do
            d <- getDepth
            let vars = [ VarP (mkName [chr (i + d + ord 'a')])
                       | i <- [0 .. length ts - 1]
                       ]
                e' = LamE vars e
            return (Just (Prim (e', c, all)))

randomMatching :: Type -> GenContext (Maybe Primitive)
randomMatching match = do
    ms <- getMatching match
    -- trace ("ms = " ++ show ms) (return ())
    case length ms of
        0 -> return Nothing
        n -> do
            i <- getRandomR (0, n-1)

            getRandomR (0, n-1) >>= return . Just . (ms !!)

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

bar = 5 :: Int

foo = $(p [| ( bar :: Int
             , map :: (a -> b) -> [a] -> [b]
             , (+) :: Int -> Int -> Int
             , id  :: a -> a
             )
           |])

t = (foo !! 2) ^. _3

t' = snd (extractPrimType t)

c = listToContext 10 foo

-- f n = let (_,g) = next (mkStdGen n)
--           s = newState g c
--       in fst (runGC (generate t) s)

-- g n = let s = newState (mkStdGen n) c
--       in fst (runGC (randR (0,1)) s)

-- h n = fst (randomR (0,2) (mkStdGen n))
