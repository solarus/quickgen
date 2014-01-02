{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Testing.QuickGen.THInstances () where

import Language.Haskell.TH.Syntax

--------------------------------------------------
-- Lift instances for template haskell types

-- TODO: Decide if this is ok or not. I.e. to define these `Lift'
-- instances for the TH types. Doing this instead of having separate
-- `lift*' functions is much more clean. I'm unsure if this is the
-- intended usage of the lift class. If this is the case then the
-- instances has to be completed!

instance Lift Exp where
    lift (VarE name)  = [| VarE name |]
    lift (AppE e1 e2) = [| AppE e1 e2 |]
    lift (ConE name)  = [| ConE name |]
    lift (TupE es)    = [| TupE es |]
    lift (LitE lit)   = [| LitE lit |]
    lift (ListE es)   = [| ListE es |]
    lift _            = error "QuickGen.THInstances.lift Exp: FIXME not all constructors are considered"

instance Lift Name where
    lift name = [| mkName $(return . LitE . StringL . nameBase $ name) |]

instance Lift Type where
    lift (ForallT ns cxt t) = [| ForallT ns cxt t |]
    lift (ConT name)        = [| ConT name |]
    lift (VarT name)        = [| VarT name |]
    lift (AppT t1 t2)       = [| AppT t1 t2 |]
    lift (TupleT n)         = [| TupleT n |]
    lift ArrowT             = [| ArrowT |]
    lift ListT              = [| ListT |]
    lift StarT              = [| StarT |]
    lift _                     = error "QuickGen.THInstances.lift Type: FIXME not all constructors are considered"

instance Lift TyVarBndr where
    lift (PlainTV name)       = [| PlainTV name |]
    lift (KindedTV name kind) = [| KindedTV name kind |]

instance Lift Pred where
    lift (ClassP name ts) = [| ClassP name ts |]
    lift (EqualP t1 t2)   = [| EqualP t1 t2 |]

instance Lift Dec where
    lift (InstanceD cxt t ds) = [| InstanceD cxt t ds |]
    lift _                    = error "QuickGen.THInstances.lift Dec: FIXME not all constructors are considered"

instance Lift Lit where
    lift (StringL s) = [| StringL s |]
    lift _           = error "QuickGen.THInstances.lift Lit: FIXME not all constructors are considered"
