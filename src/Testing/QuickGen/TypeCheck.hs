module Testing.QuickGen.TypeCheck
       ( match
       , matchInContext
       ) where

import Control.Monad
import Testing.QuickGen.Types

-- Initially I intended to use some type unification à la THIH,
-- possibly by using the implementation found in the Internal module
-- in haskell-type-exts. However, after adding the ExistsT
-- constructor, none of the standard THIH functions didn't really do
-- what I wanted. Finally I decided to implement this function
-- instead. It does something similar to type matching but can also
-- return a substitution for the first argument as well (maybe find
-- another name for this?).
match :: Monad m => Type -> Type -> m Substitution

-- Only consider the return type of a function for the second argument.
match t1 (ExistsT ns2 cxt2 (FunT (t2 : _))) = match t1 (ExistsT ns2 cxt2 t2)
match t1@(ForallT _ _ (FunT _)) _ = error $ "match: Unexpected function type " ++ show t1
match t1@(ExistsT _ _ (FunT _)) _ = error $ "match: Unexpected function type " ++ show t1

match (ExistsT _ _ t1) (ExistsT _ _ t2) = go t1 t2
  where
    go ta@(VarT _)   (VarT b)       = return (b |-> ta)

    go (VarT a)      tb@(ConT _ _)  = return (a |-> tb)

    go (VarT a)      tb@(ListT _)   = return (a |-> tb)

    go ta@(ConT _ _) (VarT b)       = return (b |-> ta)

    go (ConT n1 as1) (ConT n2 as2)  = do
        when (n1 /= n2) $ fail "Types don't match"
        mapM (uncurry go) (zip as1 as2) >>= unionsSubst

    go ta@(ConT _ _) tb@(ListT _)   = fail $ "Types don't match " ++ show ta ++ " | " ++ show tb

    go ta@(ListT _)  (VarT b)       = return (b |-> ta)

    go ta@(ListT _)  tb@(ConT _ _)  = fail $ "Types don't match " ++ show ta ++ " | " ++ show tb

    go (ListT a)     (ListT b)      = go a b

    go ta            tb             = error $ "match.go1: Not matched " ++ show ta ++ " | " ++ show tb

match (ForallT _ _ t1) (ExistsT _ _ t2) = go t1 t2
  where
    go :: Monad m => SType -> SType -> m Substitution
    go ta@(VarT _)   (VarT b)       = return (b |-> ta)

    go ta@(VarT _)   tb@(ConT _ _)  = fail $ "Types don't match " ++ show ta ++ " | " ++ show tb

    go ta@(VarT _)   tb@(ListT _)   = fail $ "Types don't match " ++ show ta ++ " | " ++ show tb

    go ta@(ConT _ _) (VarT b)       = return (b |-> ta)

    go (ConT n1 as1) (ConT n2 as2)  = do
        when (n1 /= n2) $ fail "Types don't match"
        mapM (uncurry go) (zip as1 as2) >>= unionsSubst

    go ta@(ConT _ _) tb@(ListT _)   = fail $ "Types don't match " ++ show ta ++ " | " ++ show tb

    go ta@(ListT _)  (VarT b)       = return (b |-> ta)

    go ta@(ListT _)  tb@(ConT _ _)  = fail $ "Types don't match " ++ show ta ++ " | " ++ show tb

    go (ListT a)     (ListT b)      = go a b

    go ta            tb             = error $ "match.go2: Not matched " ++ show ta ++ " | " ++ show tb

match t1 t2 = error $ "match: Not matched " ++ show t1 ++ " | " ++ show t2

matchInContext :: Type -> Context -> [(Id, Constructor, Substitution)]
matchInContext t c = (filterContextByType . match) t c
