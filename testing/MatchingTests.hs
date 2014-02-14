module MatchingTests
       ( testAll
       ) where

import Common

import Control.Monad.State

testAll :: Test
testAll = testGroup "Matching tests "
    [ testCase "match1" $ do
           let tup@(Type _ _ stup) = bindForall $ tup2 a0 e0
               expected = Just ( Type [e0, (203, Exists)] []
                                      (FunT [ stup, VarT (203, Exists), stup ])
                               , emptySubst)
           simpleMatch tup tConst expected
    ]

simpleMatch t1 t2 expected = do
    let res = runStateT (match t1 t2) emptySubst
    assertEqual "" expected res
