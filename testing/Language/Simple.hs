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


-- When template haskell was changed in 7.8 it made it seems to be
-- required to add the type signatures for functions in the language
-- definition that was defined in the same module (e.g. need to add
-- type signature for `nil' but not for `id'). This can be avoided if
-- the definition of `lang' is moved to a separate module.
lang :: Language
lang = $(defineLanguage [| ( arbiInt :: Int
                           , arbiDouble :: Double
                           , nil :: [a]
                           , cons :: a -> [a] -> [a]
                           , id
                           , foldr
                           , const
                           , sing :: a -> [a]
                           , map
                           , app :: (a -> b) -> a -> b
                           , succ :: Int -> Int
                           , succ :: Double -> Double
                           )
                         |])
