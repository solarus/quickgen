{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Testing.QuickGen.ExpGen
       ( ExpGen

       , runEG
       , getDepth
       , getRandomR
       , getRandomBool
       , localLambda
       , incUses
       , decUses
       -- , getMatching
       ) where

import           Control.Lens ((^.), (&), (%~), _1, _2)
import           Control.Monad.State
import           Data.Char (chr, ord)
import           Data.Functor ((<$>))
import qualified Data.Map as M
import           System.Random

import           Testing.QuickGen.Types

newtype ExpGen a = EG { unEG :: State (Depth, [Context], StdGen, Substitution) a }
  deriving (Functor, Monad, MonadState (Depth, [Context], StdGen, Substitution))

runEG :: ExpGen a -> Seed -> Context -> a
runEG (EG g) seed initCtx = evalState g (0, [initCtx], gen, M.empty)
  where
    gen = snd . next . mkStdGen $ seed

getDepth :: ExpGen Depth
getDepth = fmap (^. _1) get

getRandomR :: (Int, Int) -> ExpGen Int
getRandomR p = state f
  where
    f (d, cs, g, s) = let (a, g') = randomR p g in (a, (d, cs, g', s))

getRandomBool :: ExpGen Bool
getRandomBool = (==1) <$> getRandomR (0,1)

localLambda :: [Type] -> ExpGen a -> ExpGen a
localLambda ts eg = do
    (depth, cs, g, s) <- get
    let len = length ts
        uses = 10 -- FIXME: arbitrarily chosen
        c = M.fromList [ (i, (Just uses, (constr i t)))
                       | (i, t) <- zip [depth..] ts
                       ]
    put (depth + len, c : cs, g, s)
    a <- eg
    modify (& _2 %~ tail)
    return a
  where
    -- FIXME: might capture variable names
    constr i t = (mkName ("_lam_" ++ [chr (i + ord 'a')]), t)

{-

getMatching :: Type -> GenContext [Primitive]
getMatching t = GC $ do
    ps <- fmap (unContext . (^. _2)) get
    return [ Prim (e, c, ts')
           | (mn, (Prim (e, c, ts))) <- ps
           , Just ts' <- [ matchWith t (c, ts) ]
           , maybe True (> 0) mn
           ]

-}

modContext :: ([Context] -> [Context]) -> ExpGen ()
modContext f = modify (& _2 %~ f)

decUses :: Id -> ExpGen ()
decUses i = modContext (findAndUpdate f i)
  where
    f a@(Nothing, _) = a
    f (Just u, c)
        | u <= 0    = error "decUses: The impossible happened!"
        | otherwise = (Just (pred u), c)

incUses :: Id -> ExpGen ()
incUses i = modContext (findAndUpdate f i)
  where
    f (mu, c) = (succ <$> mu, c)

findAndUpdate :: ((Uses, Constructor) -> (Uses, Constructor)) -> Id -> [Context] -> [Context]
findAndUpdate f i = go
  where
    f' _ a = Just (f a)
    go [] = error $ "findAndUpdate: Id " ++ show i ++ " not found!"
    go (c:cs) = case M.updateLookupWithKey f' i c of
        (Nothing, _) -> c : go cs
        (Just _, c') -> c' : cs

{-

matches :: Type -> (Cxt, Type) -> (Bool, Maybe Name)
matches t (c, t')
    | t == t'   = (True, Nothing)
    | otherwise = case t' of
        VarT n | okCxt -> (True, Just n)
        _              -> (False, Nothing)
  where
    okCxt = True -- FIXME: Check constraints!

isVarT (VarT _) = True
isVarT _        = False

matchWith :: Type -> (Cxt, [Type]) -> Maybe [Type]
matchWith t (c, t':ts) = case t `matches` (c, t') of
    (True, Nothing) -> Just (t : ts)
    (True, Just n)  -> Just (t : ts')
      where
        ts' = map (subst n t) ts
    (False, _)      -> Nothing

subst :: Name -> Type -> Type -> Type
subst match new t@(VarT name)
    | match == name = new
    | otherwise     = t
subst match new (AppT t1 t2) = AppT (subst match new t1) (subst match new t2)
subst match new t@(ForallT ns c t') = case match `elem` map (\(PlainTV n) -> n) ns of
    True  -> t
    False -> ForallT ns c (subst match new t')
subst match new (SigT t k) = SigT (subst match new t) k
subst _ _ t = t

-}
