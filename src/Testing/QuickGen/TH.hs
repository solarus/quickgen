{-# LANGUAGE TemplateHaskell #-}

module Testing.QuickGen.TH
       ( constructors
       , printTH
       ) where

import Data.Functor ((<$>))
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Language.Haskell.TH.Syntax as TH

-- For debugging splices function
printTH e = TH.runQ e >>= print

-- | Parses a list of constructors.
constructors e = do
    TH.TupE es <- e
    (cs, es') <- unzip <$> mapM f es

    rel <- getRelevantClasses (nub (concat cs))
    rel' <- [| rel |]

    return $ TupE [rel', ListE es'] --
  where
    f e@(TH.VarE name) = do
        info <- TH.reify name

        let t = case info of
                TH.VarI _ t _ _     -> t
                TH.ClassOpI _ t _ _ -> t

        t' <- lift t
        e' <- lift e
        return (getClassnames t, (TH.TupE [e', t']))
    f (TH.SigE e@(TH.VarE name) t) = do
        e' <- lift e
        t' <- lift t
        return (getClassnames t, (TH.TupE [e', t']))

-- | Given a list of class names iteratively find new classes
-- mentioned in either the constraints of a class name or in any of
-- the instances. Returns a list with information about all instances
-- for the initial classes and discovered classes.
getRelevantClasses :: [TH.Name] -> TH.Q [(TH.Name, [TH.InstanceDec])]
getRelevantClasses = go []
  where
    go acc [] = return acc
    go acc (n:ns) = do
        d@(TH.ClassI (TH.ClassD cxt _ _ _ _) is) <- TH.reify n
        let acc' = ((n, is) : acc)
            new = nub $ [ n'
                        | TH.ClassP n' _ <- cxt ++ concat [ cxt' | TH.InstanceD cxt' _ _ <- is ]
                        , not (n' `elem` ns || isJust (lookup n' acc'))
                        ]
        go acc' (new ++ ns)

-- | Extracts the class names mentioned in a constraint in a type.
-- Returns the empty list if the type is not of the form:
-- `Pred => Type'
getClassnames :: TH.Type -> [TH.Name]
getClassnames (TH.ForallT _ cxt _) = [ n | TH.ClassP n _ <- cxt ]
getClassnames _                    = []
