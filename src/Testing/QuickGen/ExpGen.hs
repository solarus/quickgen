{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

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

import           Control.Lens ((^.), (&), (%~), (.~), _1, _2, _3, _5)
import           Control.Monad.State
import           Data.Char (chr, ord)
import           Data.Functor ((<$>))
import qualified Data.Map as M
import           Data.Maybe (listToMaybe, catMaybes)
import           System.Random

import           Testing.QuickGen.Types
import           Testing.QuickGen.TypeCheck

type LambdaDepth = Depth
type TypeDepth   = Depth

-- TODO: Make this a data type and derive lenses instead of using i.e.
-- _1 or pattern matching and the whole state when using get.
type EGState = (LambdaDepth, TypeDepth, [Context], StdGen, Substitution)

newtype ExpGen a = EG { unEG :: State EGState a }
  deriving (Functor, Monad, MonadState EGState)

generate :: Type -> Seed -> Language -> (Maybe Exp, EGState)
generate t seed ctx = case runEG (generate' t) seed ctx of
    (Just (_, e), s) -> (Just e, s)
    (Nothing,     s) -> (Nothing, s)

generate' :: Type -> ExpGen (Maybe ([Id], Exp))

-- TODO: Merge the next two patterns into one
generate' (ForallT ns cxt (FunT (t:ts))) = do
    let ts' = map (ExistsT ns cxt) ts
    (ns', ret) <- localLambda ts' (generate' (ForallT ns cxt t))
    case ret of
        Nothing       -> return Nothing
        Just (ids, e) -> return (Just (ids, LamE ns' e))

generate' (ExistsT ns cxt (FunT (t:ts))) = do
    let ts' = map (ExistsT ns cxt) ts
    (ns', ret) <- localLambda ts' (generate' (ExistsT ns cxt t))
    case ret of
        Nothing       -> return Nothing
        Just (ids, e) -> return (Just (ids, LamE ns' e))

generate' t = replicateM 2 p >>= return . listToMaybe . catMaybes
  where
    p = do
        ret <- randomMatching t
        case ret of
            Nothing -> return Nothing
            Just (i, (n, ExistsT ns cxt st), s) -> case st of
                FunT (t':ts) -> do
                    let go [] args ids        = return (True, args, ids)
                        go (t'':ts') args ids = do
                            ret <- generate' (ExistsT ns cxt t'')
                            case ret of
                                Nothing         -> return (False, args, ids)
                                Just (ids', a)  -> go ts' (a:args) (ids' ++ ids)

                    decUses i
                    (success, args, ids) <- go ts [] []
                    case success of
                        False -> mapM incUses (i:ids) >> return Nothing
                        True  -> do
                            let e' = foldl AppE (ConE n) args
                            return (Just (i:ids, e'))
                _ -> do
                    decUses i
                    modify ((& _5 %~ (maybe (error "generate'") id . unionSubst s)))
                    return (Just ([i], ConE n))

randomMatching :: Type -> ExpGen (Maybe (Id, Constructor, Substitution))
randomMatching t = do
    s <- (^. _5) <$> get
    let t' = apply s t
    matches <- getMatching t'
    case length matches of
        0 -> return Nothing
        n -> do
            (i, c, s) <- (matches !!) <$> getRandomR (0, n-1)
            Just . uncurry (i,,) <$> uniqueTypes (c, s)

getMatching :: Type -> ExpGen [(Id, Constructor, Substitution)]
getMatching t = concatMap (matchInContext t) <$> getContexts

uniqueTypes :: (Constructor, Substitution) -> ExpGen (Constructor, Substitution)
uniqueTypes ((n, t), s) = do
    td <- (^. _2) <$> get
    let ExistsT ns cxt st = t
        s' = toSubst [ (n', VarT (appendName ("_" ++ show i) n'))
                     | (i, n') <- zip [td.. ] ns
                     ]
        t' = ExistsT (catMaybes (apply s' (map Just ns))) (apply s' cxt) (apply s' st)

    modify (& _2 %~ (+ length ns))
    return ((n, t'), apply s' s)

runEG :: ExpGen a -> Seed -> Language -> (a, EGState)
-- TODO: The environment is not used yet! Need to add this to the
-- EGState.
runEG g seed (L env cs) = runState g' (0, 0, [], gen, M.empty)
  where
    g'  = unEG $ pushContext cs >> g
    gen = snd . next . mkStdGen $ seed

-- | Pushes a list of constructors to the context stack. Returns the
-- new depth and the number of constructors added.
pushContext :: [Constructor] -> ExpGen (Depth, Int)
pushContext cs = do
    (depth, td, ctxs, g, s) <- get
    let uses = 10 -- FIXME: arbitrarily chosen
        ctx = M.fromList [ (i, (Just uses, c))
                         | (i, c) <- zip [depth..] cs
                         ] :: Context
        len = M.size ctx
        depth' = depth + len
    put (depth', td, ctx : ctxs, g, s)
    return (depth', len)

popContext :: ExpGen Int
popContext = do
    (depth, td, c:cs, g, s) <- get
    let depth' = depth - M.size c
    put (depth', td, cs, g, s)
    return depth'

getDepth :: ExpGen Depth
getDepth = fmap (^. _1) get

getContexts :: ExpGen [Context]
getContexts = (^. _3) <$> get

getRandomR :: (Int, Int) -> ExpGen Int
getRandomR p = state f
  where
    f (d, td, cs, g, s) = let (a, g') = randomR p g in (a, (d, td, cs, g', s))

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

modContext :: ([Context] -> [Context]) -> ExpGen ()
modContext f = modify (& _3 %~ f)

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
