module TypesTests where

import Common

testAll :: Test
testAll = testGroup "Types tests "
    [ testProperty "No forall" noForall
    ]

noForall :: Type -> Bool
noForall t = not . hasForall . bindForall $ t

hasForall :: Type -> Bool
hasForall (Type ns _ _) = not . null $ [ () | (_, Forall) <- ns ]
