{-# LANGUAGE TemplateHaskell #-}

module GenTests
       (testAll
       ) where

import Common
import Control.Exception.Base
import GHC hiding (Type)
import GHC.Paths (libdir)
import System.Random

import Language.Simple as S

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
                 in mapM_ f ss
  where
    f s' = do
        let p = generate S.lang t s'
        putStrLn (show (fmap fst p))
        case p of
            Nothing    -> return ()
            Just (g,_) -> do
                let exprStr = "(" ++ show g ++ ") :: " ++ mt
                    go = runGhc (Just libdir) $ do
                        _ <- getSessionDynFlags >>= setSessionDynFlags
                        sequence_ [ addTarget =<< guessTarget t Nothing
                                  | t <- ("testing/Language/Simple.hs" :) $
                                         map ("src/Testing/QuickGen/"++)
                                             [ "Types.hs"
                                             , "THInstances.hs"
                                             , "TH.hs"
                                             ]
                                  ]
                        load LoadAllTargets

                        setContext [ IIDecl . simpleImportDecl . mkModuleName $ "Prelude"
                                   , IIDecl . simpleImportDecl . mkModuleName $ "Language.Simple"
                                   ]
                        r <- compileExpr exprStr
                        r `seq` return (Right r)

                res <- catch go $ \e -> return (Left (e :: SomeException))
                case res of
                    Left  e -> assertFailure $
                        "Failure for expression:\n" ++ exprStr ++ "\n" ++ show e
                    Right _ -> return ()
