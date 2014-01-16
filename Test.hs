{-# LANGUAGE FlexibleInstances, TemplateHaskell, TypeSynonymInstances #-}

module Main where

import qualified Control.Monad.Error as E
import           Control.Monad.Identity
import           Data.Functor
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           System.Environment
import           Text.Groom

import           Testing.QuickGen
import           Testing.QuickGen.Types
import           Testing.QuickGen.TypeCheck

main :: IO ()
main = do
    seed <- maybe 0 read . listToMaybe <$> getArgs
    main' seed

main' seed = do
    -- [a,b] <- fmap (map (read :: String -> Int)) getArgs
    -- print (maximumBy (comparing length) (map f [a..b]))
    -- putStrLn . pprint =<< foo

    let keys = M.keys env
    putStrLn (groom env)
    putStrLn $ "Number of classes: " ++ show (length keys)
    mapM_ print keys
    putStrLn "\nTrivial type matching tests:"
    mapM_ print typeTests

    putStrLn "\nTrivial substitution test:"
    print applyTest

    putStrLn "\nTrivial generation test:"
    print (genTest seed)

-- Should generate expressions of type "Int -> Double -> [Int]"
genTest seed = fst $ generate (ForallT [] [] (FunT [ListT tInt, tDouble, tInt])) seed lang

tInt    = tCon "Int"
tDouble = tCon "Double"

applyTest = let a0 = mkName "a0"
                a2 = mkName "a2"
                b1 = mkName "b1"
                eq n = ClassP (mkName "Eq") [VarT n]
                t1 = ExistsT [a0,b1] [eq a0, eq b1] (ListT (VarT a0))
                s  = toSubst [(a0, VarT a2)]
            in (t1, apply s t1)

genInt :: Int
genInt = 5

genDouble :: Double
genDouble = 6

nil :: [a]
nil = []

cons :: a -> [a] -> [a]
cons = (:)

lang :: Language
env  :: ClassEnv
cs   :: [ Constructor ]
lang@(L env cs) = $(defineLanguage [| ( genInt
                                      , genDouble
                                      , nil
                                      , cons
                                      , id
                                      -- , (+)
                                      -- , (*)
                                      -- , const
                                      -- , (/)
                                      , map
                                      -- , either
                                      )
                                    |])

tCon :: String -> SType
tCon a = ConT (mkName a) []

a',b',c' :: Name
[a', b', c'] = map mkName (map (:[]) "abc")

conEither :: Name -> Name -> SType
conEither a b = ConT (mkName "Either") [VarT a, VarT b]

(-->) :: Name -> Name -> SType
a --> b = FunT [VarT b, VarT a]

tA :: Type
tA = ForallT [a'] [] (VarT a')

tListA :: Type
tListA = ForallT [a'] [] (ListT (VarT a'))

-- The type of id :: a -> a
tId :: Type
tId = ExistsT [a'] [] (FunT [ VarT a', VarT a' ])

-- The type of either :: (a -> c) -> (b -> c) -> Either a b -> c
tEither :: Type
tEither = ExistsT [a', c', b'] [] (FunT [VarT c', conEither a' b', b' --> c', a' --> c'])

-- The type of map :: (a -> b) -> [a] -> [b]
tMap :: Type
tMap  = ExistsT [a', b'] [] (FunT [ ListT (VarT b'), ListT (VarT a'), a' --> b' ])

exists :: String -> Type
exists a = let na = mkName a in ExistsT [na] [] (VarT na)

-- TODO: Better and more tests for the type matching
typeTests :: [ Error Substitution ]
typeTests = [ match tA tId
            , match tA tEither
            , match tListA tId
            , match tListA tMap
            , match (exists "b") (ExistsT [] [] (ListT (tCon "Int")))
            , match (exists "c") tMap
            , match (exists "c") tId
            ]

{-

-- the type `Int -> Int -> Int'
t1 = AppT (AppT ArrowT (ConT (mkName "Int"))) (AppT (AppT ArrowT (ConT (mkName "Int"))) (ConT (mkName "Int")))

t1' = snd (extractPrimType t1)

t2 = ([] , [ VarT (mkName "b")
           , VarT (mkName "a")
           , AppT (AppT ArrowT (VarT (mkName "a"))) (VarT (mkName "b"))
           ])

c = listToContext 10 foo

f n = let Just e = generate t1 n c in pprint e

g from to = maximumBy (comparing length) (map f [from..to])

baz :: (forall a. a) -> (forall a. a)
baz a = a

id2 = id

test = matchWith (head t1') t2

bar :: Integral a => a -> a -> a
bar a b = a + b

-}

type Error a = E.ErrorT String Identity a

instance Show a => Show (Error a) where
    show e = case runIdentity (E.runErrorT e) of
        Left err -> "ERROR: " ++ err
        Right ok -> "OK: " ++ show ok
