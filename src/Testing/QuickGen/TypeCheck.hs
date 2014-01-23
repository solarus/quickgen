module Testing.QuickGen.TypeCheck
       ( match
       , matchInContext
       ) where

import Control.Monad
import Data.Functor
import Data.Maybe
import Testing.QuickGen.Types

-- Initially I intended to use some type unification à la THIH,
-- possibly by using the implementation found in the Internal module
-- in haskell-type-exts. However, after adding the ExistsT
-- constructor, none of the standard THIH functions didn't really do
-- what I wanted. Finally I decided to implement this function
-- instead. It does something similar to type matching but can also
-- return a substitution for the first argument as well (maybe find
-- another name for this?).
match :: (Functor m, Monad m) => Type -> Type -> m (Maybe Substitution, Type)

-- Only consider the return type of a function for the second argument.
match t1 t2 = do
    (ms1, ms2) <- match' t1 t2
    let (Type _ cxt st) = case ms2 of
            Nothing -> t2
            Just s2 -> apply s2 t2
        vars = getVars st
        f n = case toQuantifier n t1 of
            Just q  -> q
            Nothing -> Exists n
        t' = Type (map f vars) cxt st
    return (ms1, t')

match' :: (Functor m, Monad m) => Type -> Type -> m (Maybe Substitution, Maybe Substitution)
match' t1@(Type _ _ (FunT _)) _ = error $ "match: Unexpected function type " ++ show t1
match' t1 (Type ns cxt (FunT (t2 : _))) = match' t1 (Type ns cxt t2)
match' ta@(Type ns1 _ st1) tb@(Type _ _ st2) = go st1 st2
  where
    getVar _ [] = error "match: The impossible happened!"
    getVar v (q:qs)
        | v == getName q = q
        | otherwise      = getVar v qs

    go t1@(VarT _) (VarT n2) = return (Nothing, Just (n2 |-> t1))
    go t1          (VarT n2) = return (Nothing, Just (n2 |-> t1))
    go (VarT n1)   t2
        | isExists (getVar n1 ns1) = return (Just (n1 |-> t2), Nothing)
        | otherwise                = fail "No match"

    go (ConT n1 as1) (ConT n2 as2)
        | n1 /= n2  = fail "Types don't match!"
        | otherwise = do
            (s1, s2) <- both catMaybes . unzip <$> zipWithM go as1 as2
            let f [] = return Nothing
                f xs = Just <$> unionsSubst xs
            liftM2 (,) (f s1) (f s2)

    go (ConT _ _)    _          = fail "Types don't match!"

    go (ListT t1)    (ListT t2) = go t1 t2

    go (ListT _)     _          = fail "Types don't match!"

    go _ _ = error $ "match: Not matched " ++ show ta ++ " | " ++ show tb

matchInContext :: Type -> Context -> [(Id, Constructor, Maybe Substitution)]
matchInContext t1 c = foldrContext f [] c
  where
    f i (u, (n, t2)) acc = case match t1 t2 of
        Just (ms, t') | maybe True (>0) u -> (i, (n, t'), ms) : acc
        _                                 -> acc

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) =  (f a, f b)
