{-# LANGUAGE TemplateHaskell #-}

module GenTests
       (testAll
       ) where

import Common
import Control.Monad (replicateM)
import Data.Functor

testAll :: Seed -> Test
testAll s = testGroup "Generation tests"
    [ testCase "gen1" $ testGen s (Type [] [] (ListT tInt))
    ]

testGen :: Seed -> Type -> Assertion
testGen s t = do
    res <- retry 3 . return . fst $ generate lang t s
    assertBool ("Failed to generate  " ++ show t) (isJust res)


retry :: (Monad f, Functor f) => Int -> f a -> f (Maybe a)
retry n m = listToMaybe <$> replicateM n m

lang :: Language
lang = $(defineLanguage [| ( genInt
                           , genDouble
                           , nil
                           , cons
                           , id
                           , foldr
                           , const
                           , map
                           )
                         |])
  where
    genInt    = 1 :: Int
    genDouble = 2.5 :: Double
    nil = []
    cons = (:)
