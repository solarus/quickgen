{-# LANGUAGE TemplateHaskell #-}

module GenTests
       (testAll
       ) where

import Common
import Control.Exception.Base
import GHC hiding (Type)
import GHC.Paths (libdir)
import System.Random

import Language.Simple

testAll :: Seed -> Test
testAll s = testGroup "Generation tests "
    [ testCase "generate expressions of type :: a -> [a]" $
        testGen s (Type [a] [] (FunT [ListT (VarT a), VarT a])) "a -> [a]"
    , testCase "generate expressions of type :: [Int]" $
        testGen s (Type [] [] (ListT tInt)) "[Int]"
    ]
  where
    a = (0, Forall)

testGen :: Seed -> Type -> String -> Assertion
testGen s t mt = let gen = mkStdGen s
                     ss  = take 50 (randomRs (0,1000000) gen)
                 in mapM_ p ss
  where
    p s' = do
        let (g, _) = generate lang t s'
        putStrLn (show g)
        case g of
            Nothing -> return ()
            Just g' -> do
                let exprStr = unwords [ "let genInt = 0 :: Int;"
                                      , "genDouble = 1 :: Double;"
                                      , "nil = [];"
                                      , "cons = (:)"
                                      , "in (" ++ show g' ++ ") :: " ++ mt
                                      ]
                    go = runGhc (Just libdir) $ do
                        _ <- getSessionDynFlags >>= setSessionDynFlags
                        setContext [ IIDecl . simpleImportDecl . mkModuleName $ "Prelude" ]
                        r <- compileExpr exprStr
                        r `seq` return (Right r)

                res <- catch go $ \e -> return (Left (e :: SomeException))
                case res of
                    Left  e -> assertFailure $
                        "Failure for expression:\n" ++ exprStr ++ "\n" ++ show e
                    Right _ -> return ()
