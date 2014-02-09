module Common
       ( module Data.Maybe
       , module Test.Framework
       , module Test.Framework.Providers.HUnit
       , module Test.HUnit
       , module Testing.QuickGen.ExpGen
       , module Testing.QuickGen.TH
       , module Testing.QuickGen.Types


       -- , assertBool
       -- , assertFailure
       -- , assertEqual

       , a0, a1, a2, a3, a4
       , e0, e1, e2, e3, e4

       , tInt

       , (-->)
       , tMap
       , tConst
       , tup2
       ) where

import Data.Maybe
import Test.Framework hiding (Seed)
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, assertBool, assertFailure, assertEqual)
import Testing.QuickGen.ExpGen
import Testing.QuickGen.TH
import Testing.QuickGen.Types

a0,a1,a2,a3,a4,e0,e1,e2,e3,e4 :: Variable
a0 = (0, Forall)
a1 = (1, Forall)
a2 = (2, Forall)
a3 = (3, Forall)
a4 = (4, Forall)
e0 = (10, Exists)
e1 = (11, Exists)
e2 = (12, Exists)
e3 = (13, Exists)
e4 = (14, Exists)

tInt :: SType
tInt = ConT (mkName "Int") []

(-->) :: Variable -> Variable -> SType
a --> b = FunT [VarT b, VarT a]

tMap :: Type
tMap = Type [a, b] [] (FunT [ ListT (VarT b), ListT (VarT a), a --> b ])
  where a = (100, Forall)
        b = (101, Forall)

tConst :: Type
tConst = Type [a, b] [] (FunT [ VarT a, VarT b, VarT a])
  where a = (102, Forall)
        b = (103, Forall)

tup2 :: Variable -> Variable -> Type
tup2 v1 v2 = Type [v1,v2] [] (ConT (mkName "Tup2") [VarT v1, VarT v2])
