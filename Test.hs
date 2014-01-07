{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- import           Data.List
import qualified Data.Map.Strict as M
-- import           System.Environment
import           Testing.QuickGen

import Text.Groom
import Testing.QuickGen.Types

main :: IO ()
main = do
    -- [a,b] <- fmap (map (read :: String -> Int)) getArgs
    -- print (maximumBy (comparing length) (map f [a..b]))
    -- putStrLn . pprint =<< foo

    let keys = M.keys env
    putStrLn $ "Number of classes: " ++ show (length keys)
    mapM_ print keys

    putStrLn (groom env)

genInt :: Int
genInt = 5

env :: ClassEnv
cs  :: [ Constructor ]
L env cs = $(defineLanguage [| ( id
                               , (+)
                               , (*)
                               )
                             |])

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
