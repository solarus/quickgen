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
