{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes #-}

module Testing.QuickGen where

import Data.Typeable
import Test.Feat
-- import Test.Feat.Enumerate

data Type :: * -> * where
    Int  :: Type Int
    List :: Type a -> Type [a]
    Func :: Type a -> Type b -> Type (a -> b)

data Typed a = a ::: Type a

data Spine :: * -> * where
    Constr :: a -> Spine a
    (:<>:) :: Spine (a -> b) -> Typed a -> Spine b
  deriving Typeable

fromSpine :: Spine a -> a
fromSpine (Constr x) = x
fromSpine (c :<>: (x ::: _)) = fromSpine c x

vint = (::: Int)

foo = Constr (+) :<>: vint 1 :<>: vint 5

instance (Typeable a, Enumerable a) => Enumerable (Spine a) where
    enumerate = consts [ unary Constr
                       , funcurry undefined ]

-- dynVal :: Typeable a => Dynamic -> a
-- dynVal a = fromDyn a undefined

-- bar :: [Dynamic]
-- bar = [toDyn (Fun ((+) :: Int -> Int -> Int)), toDyn (Fun (1 :: Int))]

-- foo :: Bool
-- foo = fromSpine (dynVal (bar !! 1))

-- bar ::
