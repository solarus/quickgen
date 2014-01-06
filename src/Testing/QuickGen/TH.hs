{-# LANGUAGE TemplateHaskell #-}

module Testing.QuickGen.TH
       ( defineLanguage
       ) where

import           Control.Monad ((>=>))
import           Data.List (nub)
import           Data.Maybe (isJust)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (lift)

import           Testing.QuickGen.THInstances ()
import           Testing.QuickGen.Types as T

defineLanguage :: TH.ExpQ -> TH.ExpQ
defineLanguage tup = do
    TH.TupE es <- tup
    defineLanguage' (map f es) >>= (lift >=> return)
  where
    f (TH.VarE name)             = (name, Nothing)
    f (TH.SigE (TH.VarE name) t) = (name, Just t)

defineLanguage' :: [(Name, Maybe TH.Type)] -> TH.Q Language
defineLanguage' es = do
    cs <- mapM f es
    let ns = nub (concatMap (getClassNames . snd) cs)
    env <- getClassEnv ns
    return $ L env cs
  where
    f :: (Name, Maybe TH.Type) -> TH.Q Constructor
    f (name, mt) = do
        t <- case mt of
            Nothing -> do
                info <- TH.reify name
                case info of
                    TH.VarI     _ t' _ _ -> return t'
                    TH.ClassOpI _ t' _ _ -> return t'
            Just t' -> return t'
        let t' = thTypeToType t
        return (name, t')

-- | Given a list of class names iteratively find new classes
-- mentioned in either the constraints of a class name or in any of
-- the instances. Returns the `ClassEnv' with information about all
-- instances for the initial classes and the discovered classes.
getClassEnv :: [Name] -> TH.Q ClassEnv
getClassEnv = go empty
  where
    go :: ClassEnv -> [Name] -> TH.Q ClassEnv
    go acc [] = return acc
    go acc (n:ns) = do
        TH.ClassI (TH.ClassD cxt _ _ _ _) is <- TH.reify n
        let cxt' = thCxtToCxt cxt
            acc' = insert n (getCxtNames cxt', is) acc
            is'  = concatMap thCxtToCxt [ c | TH.InstanceD c _ _ <- is ]
            new  = nub $ [ n'
                         | n' <- getCxtNames (cxt' ++ is')
                         , not (n' `elem` ns || isJust (T.lookup n' acc'))
                         ]
        go acc' (new ++ ns)
