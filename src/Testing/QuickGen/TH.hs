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


--------------------------------------------------
-- Lift instances for template haskell types

-- TODO: Decide if this is ok or not. I.e. to define these `Lift'
-- instances for the TH types. Doing this instead of having separate
-- `lift*' functions is much more clean. I'm unsure if this is the
-- intended usage of the lift class. If this is the case then the
-- instances has to be completed!

instance Lift Exp where
    lift (VarE name)  = [| TH.VarE name |]
    lift (AppE e1 e2) = [| AppE e1 e2 |]
    lift (ConE name)  = [| ConE name |]
    lift (TupE es)    = [| TupE es |]
    lift (LitE lit)   = [| LitE lit |]
    lift (ListE es)   = [| ListE es |]
    lift _            = error "QuickGen.TH.lift Exp: FIXME not all constructors are considered"

instance Lift Name where
    lift name = [| TH.mkName $(return . TH.LitE . TH.StringL . TH.nameBase $ name) |]

instance Lift Type where
    lift (TH.ForallT ns cxt t) = [| TH.ForallT ns cxt t |]
    lift (TH.ConT name)        = [| TH.ConT name |]
    lift (TH.VarT name)        = [| TH.VarT name |]
    lift (TH.AppT t1 t2)       = [| TH.AppT t1 t2 |]
    lift (TH.TupleT n)         = [| TH.TupleT n |]
    lift TH.ArrowT             = [| TH.ArrowT |]
    lift TH.ListT              = [| TH.ListT |]
    lift TH.StarT              = [| TH.StarT |]
    lift _                     = error "QuickGen.TH.lift Type: FIXME not all constructors are considered"

instance Lift TyVarBndr where
    lift (PlainTV name)       = [| PlainTV name |]
    lift (KindedTV name kind) = [| KindedTV name kind |]

instance Lift Pred where
    lift (TH.ClassP name ts) = [| TH.ClassP name ts |]
    lift (TH.EqualP t1 t2)   = [| TH.EqualP t1 t2 |]

instance Lift Dec where
    lift (InstanceD cxt t ds) = [| InstanceD cxt t ds |]
    lift _                    = error "QuickGen.TH.lift Dec: FIXME not all constructors are considered"

instance Lift Lit where
    lift (StringL s) = [| StringL s |]
    lift _           = error "QuickGen.TH.lift Lit: FIXME not all constructors are considered"
