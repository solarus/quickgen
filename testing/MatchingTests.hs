module MatchingTests
       ( testAll
       ) where

import Common

import Control.Monad.State

testAll :: Test
testAll = testGroup "Matching tests"
    [ testCase "match1" $ do
           let tup@(Type _ _ stup) = tup2 a0 e0
               e103 = (103, Exists)
               expected = Just ( Type [a0, e0, e103] [] (FunT [ stup, VarT e103, stup ])
                               , emptySubst)
           simpleMatch tup tConst expected
    ]

simpleMatch t1 t2 expected = do
    let res = runStateT (match t1 t2) emptySubst
    assertEqual "" res expected
