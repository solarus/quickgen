module Main (main) where

import Data.List
import Data.Ord
import System.Environment

import Testing.QuickGen

main = do
    [a,b] <- fmap (map read) getArgs
    print (maximumBy (comparing length) (map f [a..b]))
