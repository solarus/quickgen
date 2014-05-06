{-# LANGUAGE TemplateHaskell #-}

module Language.Simple where

import Testing.QuickGen.TH
import Testing.QuickGen.Types

genInt :: Int
genInt = 0

genDouble :: Double
genDouble = 1 :: Double

nil :: [a]
nil = []

cons :: a -> [a] -> [a]
cons = (:)

sing :: a -> [a]
sing x = [x]

app :: (a -> b) -> a -> b
app f x = f x

inc :: Int -> Int
inc n = n + 1

lang :: Language
lang = $(defineLanguage [| ( genInt
                           , genDouble
                           , nil
                           , cons
                           , id
                           , foldr
                           , const
                           , sing
                           , map
                           , app
                           , inc
                           )
                         |])
