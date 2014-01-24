{-# LANGUAGE FlexibleInstances, TemplateHaskell, TypeSynonymInstances #-}

module Main where

import qualified Control.Monad.Error as E
import           Control.Monad.Identity
import           Data.Functor
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Ord
import           System.Environment
import           Text.Groom

import           Testing.QuickGen
import           Testing.QuickGen.ExpGen
import           Testing.QuickGen.Types
import           Testing.QuickGen.TypeCheck

main :: IO ()
main = do
    args <- map (read :: String -> Int) <$> getArgs
    let (low, high) = case args of
            low' : high' : [] -> (low', high')
            _                 -> (0, 100)
    main' low high

main' low high = do
    let keys = M.keys env
    putStrLn (groom env)
    putStrLn $ "Number of classes: " ++ show (length keys)
    mapM_ print keys
    putStrLn "\nTrivial type matching tests:"
    mapM_ print typeTests

    putStrLn "\nTrivial substitution test:"
    print applyTest

    putStrLn "\nTrivial generation test:"
    putStrLn (maximumBy (comparing length) . map (show . fst . genTest) $ [low .. high])

-- Should generate expressions of type "Int -> Double -> [Int]"
genTest seed = generate (Type [] [] (FunT [ListT tInt, tDouble, tInt])) seed lang

tInt    = tCon "Int"
tDouble = tCon "Double"

applyTest = let a0 = mkName "a0"
                a2 = mkName "a2"
                b1 = mkName "b1"
                eq n = ClassP (mkName "Eq") [VarT n]
                t1 = Type [Exists a0, Exists b1] [eq a0, eq b1] (ListT (VarT a0))
                s  = toSubst [(a0, ([Exists a2], VarT a2))]
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


(-->) :: Name -> Name -> SType
a --> b = FunT [VarT b, VarT a]

tA :: Type
tA = Type [Forall a] [] (VarT a)
  where a = mkName "a_1"

tListA :: Type
tListA = Type [Forall a] [] (ListT (VarT a))
  where a = mkName "a_2"

-- The type of id :: a -> a
tId :: Type
tId = Type [Exists a] [] (FunT [ VarT a, VarT a ])
  where a = mkName "a_3"

-- The type of either :: (a -> c) -> (b -> c) -> Either a b -> c
tEither :: Type
tEither = Type (map Exists [a, c, b]) [] (FunT [VarT c, either_a_b, b --> c, a --> c])
  where a = mkName "a_4"
        b = mkName "b_5"
        c = mkName "c_6"
        either_a_b = ConT (mkName "Either") [VarT a, VarT b]

-- The type of map :: (a -> b) -> [a] -> [b]
tMap :: Type
tMap = Type (map Exists [a, b]) [] (FunT [ ListT (VarT b), ListT (VarT a), a --> b ])
  where a = mkName "a_7"
        b = mkName "b_8"

exists :: String -> Type
exists a = let na = mkName a in Type [Exists na] [] (VarT na)

forall :: String -> Type
forall a = let na = mkName a in Type [Forall na] [] (VarT na)

-- TODO: Better and more tests for the type matching
typeTests :: [ Error (Maybe Substitution, Type) ]
typeTests = [ match tA tId
            , match (Type [] [] tInt) tEither
            , match tListA tId
            , match tListA tMap
            , match (Type [] [] (ListT (tCon "Int"))) tMap
            , match (exists "b") (Type [] [] (ListT (tCon "Int")))
            , match (exists "c") tMap
            , match (exists "c") tId
            , match (exists "a") (Type [] [] tDouble)
            ]

type Error a = E.ErrorT String Identity a

instance Show a => Show (Error a) where
    show e = case runIdentity (E.runErrorT e) of
        Left err -> "ERROR: " ++ err
        Right ok -> "OK: " ++ show ok
