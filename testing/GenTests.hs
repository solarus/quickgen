{-# LANGUAGE TemplateHaskell #-}

module GenTests
       (testAll
       ) where

import Common
import Control.Exception.Base
import Data.Dynamic
import GHC hiding (Type)
import GHC.Paths (libdir)
import System.Random

import Language.Simple

testAll :: Seed -> Test
testAll s = testGroup "Generation tests "
    [ testCase "gen1" $ testGen s (Type [] [] (ListT tInt))
    ]

testGen :: Seed -> Type -> Assertion
testGen s t = let gen = mkStdGen s
                  ss  = take 100 (randomRs (0,10000) gen)
              in mapM_ p ss
  where
    p s' = do
        case fst (generate lang t s') of
            Nothing -> return ()
            Just g  -> do
                let exprStr = unwords [ "let genInt = 0 :: Int;"
                                      , "genDouble = 1 :: Double;"
                                      , "nil = [];"
                                      , "cons = (:)"
                                      , "in (" ++ show g ++ ")"
                                      , "`asTypeOf` (undefined :: [Int])"
                                      ]

                    go = do
                        r <- runGhc (Just libdir) $ do
                            _ <- getSessionDynFlags >>= setSessionDynFlags
                            setContext [ IIDecl . simpleImportDecl . mkModuleName $ "Prelude" ]
                            dynCompileExpr exprStr
                        let expr = fromDyn r (error "wat" :: [Int])
                        return (Right expr)

                res <- catch go $ \e -> return (Left (e :: SomeException))
                case res of
                    Left  e -> assertFailure $
                        "Failure for expression:\n" ++ exprStr ++ "\n" ++ show e
                    Right _ -> return ()
