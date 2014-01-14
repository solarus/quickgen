{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Testing.QuickGen.ExpGen
       ( ExpGen

       , generate
       , runEG
       , getDepth
       , getRandomR
       , getRandomBool
       , localLambda
       , incUses
       , decUses
       -- , getMatching
       ) where

import           Control.Lens ((^.), (&), (%~), (.~), _1, _2)
import           Control.Monad.State
import           Data.Char (chr, ord)
import           Data.Functor ((<$>))
import qualified Data.Map as M
import           System.Random

import           Testing.QuickGen.Types
import           Testing.QuickGen.TypeCheck

type EGState = (Depth, [Context], StdGen, Substitution)

newtype ExpGen a = EG { unEG :: State EGState a }
  deriving (Functor, Monad, MonadState EGState)

generate :: Type -> Seed -> Language -> (Maybe Exp, EGState)
generate t seed ctx = case runEG (generate' t) seed ctx of
    (Just (_, e), s) -> (Just e, s)
    (Nothing,     s) -> (Nothing, s)

generate' :: Type -> ExpGen (Maybe ([Id], Exp))
generate' (ForallT ns cxt (FunT (t:ts))) = do
    let ts' = map (ExistsT ns cxt) ts
    (ns', ret) <- localLambda ts' (generate' (ForallT ns cxt t))
    case ret of
        Nothing       -> return Nothing
        Just (ids, e) -> return (Just (ids, LamE ns' e))
generate' t@(ForallT _ _ _) = do
    ret <- randomMatching t
    return $ case ret of
        Nothing          -> Nothing
        Just (_, (n, _)) -> Just ([], ConE n)

randomMatching :: Type -> ExpGen (Maybe (Id, Constructor))
randomMatching t = do
    matches <- getMatching t
    case length matches of
        0 -> return Nothing
        n -> do
            idx <- getRandomR (0, n-1)
            let (i, c, _s) = matches !! idx
            return (Just (i, c))

runEG :: ExpGen a -> Seed -> Language -> (a, EGState)
-- TODO: The environment is not used yet! Need to add this to the
-- EGState.
runEG g seed (L env cs) = runState g' (0, [], gen, M.empty)
  where
    g'  = unEG $ pushContext cs >> modify (& _1 .~ 0) >> g
    gen = snd . next . mkStdGen $ seed

-- | Pushes a list of constructors to the context stack. Returns the
-- new depth and the number of constructors added.
pushContext :: [Constructor] -> ExpGen (Depth, Int)
pushContext cs = do
    (depth, ctxs, g, s) <- get
    let uses = 10 -- FIXME: arbitrarily chosen
        ctx = M.fromList [ (i, (Just uses, c))
                         | (i, c) <- zip [depth..] cs
                         ] :: Context
        len = M.size ctx
        depth' = depth + len
    put (depth', ctx : ctxs, g, s)
    return (depth', len)

popContext :: ExpGen Int
popContext = do
    (depth, c:cs, g, s) <- get
    let depth' = depth - M.size c
    put (depth', cs, g, s)
    return depth'

getDepth :: ExpGen Depth
getDepth = fmap (^. _1) get

getContexts :: ExpGen [Context]
getContexts = (^. _2) <$> get

getRandomR :: (Int, Int) -> ExpGen Int
getRandomR p = state f
  where
    f (d, cs, g, s) = let (a, g') = randomR p g in (a, (d, cs, g', s))

getRandomBool :: ExpGen Bool
getRandomBool = (==1) <$> getRandomR (0,1)

localLambda :: [Type] -> ExpGen a -> ExpGen ([Name], a)
localLambda ts eg = do
    depth <- getDepth
    let cs = map constr (zip [depth..] ts)
        ns = map fst cs

    pushContext cs
    a <- eg
    popContext

    return (ns, a)
  where
    -- FIXME: might capture variable names
    constr (i, t) = (mkName ("_lam_" ++ [chr (i + ord 'a')]), t)

getMatching :: Type -> ExpGen [(Id, Constructor, Substitution)]
getMatching t = concatMap (matchInContext t) <$> getContexts

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
