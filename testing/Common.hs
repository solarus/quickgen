{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Common
       ( module Data.Maybe
       , module Test.Framework
       , module Test.Framework.Providers.HUnit
       , module Test.Framework.Providers.QuickCheck2
       , module Test.HUnit
       , module Testing.QuickGen.ExpGen
       , module Testing.QuickGen.TH
       , module Testing.QuickGen.Types

       , a0, a1, a2, a3, a4
       , e0, e1, e2, e3, e4

       , tInt

       , (-->)
       , tMap
       , tConst
       , tup2
       ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import Test.Framework hiding (Seed)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (Assertion, assertBool, assertFailure, assertEqual)
import Test.QuickCheck
import Testing.QuickGen.ExpGen
import Testing.QuickGen.TH
import Testing.QuickGen.Types

a0,a1,a2,a3,a4,e0,e1,e2,e3,e4 :: Variable
a0 = (100, Forall)
a1 = (101, Forall)
a2 = (102, Forall)
a3 = (103, Forall)
a4 = (104, Forall)
e0 = (110, Undecided)
e1 = (111, Undecided)
e2 = (112, Undecided)
e3 = (113, Undecided)
e4 = (114, Undecided)

tInt :: SType
tInt = ConT (mkName "Int") []

(-->) :: Variable -> Variable -> SType
a --> b = FunT [VarT b, VarT a]

tMap :: Type
tMap = Type [a, b] [] (FunT [ ListT (VarT b), ListT (VarT a), a --> b ])
  where a = (200, Forall)
        b = (201, Forall)

tConst :: Type
tConst = Type [a, b] [] (FunT [ VarT a, VarT b, VarT a])
  where a = (202, Forall)
        b = (203, Forall)

tup2 :: Variable -> Variable -> Type
tup2 v1 v2 = Type [v1,v2] [] (ConT (mkName "Tup2") [VarT v1, VarT v2])

instance Arbitrary Quantifier where
    arbitrary = elements [ Forall, Undecided ]

instance Arbitrary Name where
    arbitrary = mkName <$> (getNonEmpty <$> arbitrary)

newtype Var = Var { fromVar :: Variable }

instance Arbitrary Var where
    arbitrary = do
        n <- getNonNegative <$> arbitrary
        q <- arbitrary
        return (Var (n, q))

instance Arbitrary SType where
    arbitrary = sized $ \n -> choose (0, min 1000 n) >>= go
      where
        go n = frequency [ (n, fun)
                         , (5, var)
                         , (n, con)
                         , (n*2, list)
                         ]
          where
            n' = n `div` 2
            var = VarT . fromVar <$> arbitrary
            fun = do
                k <- choose (2, 4)
                FunT <$> replicateM k (go n')
            con = do
                (k, name) <- elements . map (second mkName) $
                               [ (0, "Int")
                               , (0, "Double")
                               , (2, "Tup2")
                               , (3, "Tup3")
                               , (1, "Maybe")
                               ]
                ConT name <$> replicateM k (go n')
            list = ListT <$> go n'

instance Arbitrary Type where
    arbitrary = do
        st <- arbitrary
        return $ Type (getVars st) [] st
