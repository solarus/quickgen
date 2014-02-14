module Main where

import Common
import Data.Functor
import GenTests
import MatchingTests
import System.Environment
import System.Random
import TypesTests

main :: IO ()
main = do
    args <- interpretArgsOrExit =<< getArgs
    let testSeed = maybe RandomSeed id (topt_seed =<< ropt_test_options args)
    seed <- fst . randomR (0, 1000000) . fst <$> newSeededStdGen testSeed

    let allTests = [ MatchingTests.testAll
                   , GenTests.testAll seed
                   , TypesTests.testAll
                   ]

    defaultMain allTests
