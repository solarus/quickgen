{-# LANGUAGE TemplateHaskell #-}

module Language.Simple where

import Testing.QuickGen.TH
import Testing.QuickGen.Types

arbiInt :: Int
arbiInt = 0

arbiDouble :: Double
arbiDouble = 1

nil :: [a]
nil = []

cons :: a -> [a] -> [a]
cons = (:)

sing :: a -> [a]
sing x = [x]

app :: (a -> b) -> a -> b
app f x = f x

succInt :: Int -> Int
succInt = succ

succDouble :: Double -> Double
succDouble = succ

lang :: Language
lang = $(defineLanguage [| ( arbiInt
                           , arbiDouble
                           , nil
                           , cons
                           , id
                           , foldr
                           , const
                           , sing
                           , map
                           , app
                           , succInt
                           , succDouble
                           )
                         |])
