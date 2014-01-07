{-# LANGUAGE TemplateHaskell #-}

module Testing.QuickGen.TH
       ( defineLanguage
       ) where

import           Control.Monad ((>=>))
import           Data.List (nub)
import           Data.Maybe (isNothing)
import           Data.Set (Set)
import qualified Data.Set as S
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
getClassEnv = go empty . S.fromList
  where

    go :: ClassEnv -> Set Name -> TH.Q ClassEnv
    go acc s
        | S.null s                = return acc
        | otherwise               = do
            TH.ClassI (TH.ClassD cxt _ _ _ _) is <- TH.reify n
            let cxt' = toNameSet cxt
                acc' = insert n (S.toList cxt', is) acc
                is'  = map (toNameSet . (\(TH.InstanceD c _ _) -> c)) is
                -- TODO: If I try to find all classes then when
                -- splicing in the resulting expression it can take a
                -- minute or more to compile! Is there some way around
                -- this? How to solve this. Maybe let the user specify
                -- all type classes they want to consider instead? For
                -- the moment only look in the super classes.
                new  = [cxt'] -- cxt' : is'
            go acc' (S.unions (s' : new))
      where
        (n,s') = S.deleteFindMin s
        toNameSet :: TH.Cxt -> Set Name
        toNameSet c = S.fromList (filter (isNothing . (`T.lookup` acc)) (map (\(TH.ClassP n _) -> n) c))
