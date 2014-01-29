{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Testing.QuickGen.ExpGen
       ( ExpGen

       , generate
       , runEG
       , nextLambda
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
import           Data.Maybe (catMaybes, listToMaybe)
import           System.Random

import           Testing.QuickGen.Types
import           Testing.QuickGen.TypeCheck

type NextLambda = Nat
type NextType   = Nat

-- TODO: Make this a data type and derive lenses instead of using i.e.
-- _1 or pattern matching and the whole state when using get.
type EGState = (NextLambda, NextType, [Context], StdGen, Substitution)

newtype ExpGen a = EG { unEG :: State EGState a }
  deriving (Functor, Monad, MonadState EGState)

generate :: Type -> Seed -> Language -> (Maybe Exp, EGState)
generate t seed ctx = runEG (generate' t) seed ctx

generate' :: Type -> ExpGen (Maybe Exp)

generate' (Type qs cxt (FunT (t:ts))) = do
    let -- TODO: clean up qs and cxt?
        ts' = map (Type qs cxt) ts
        t'  = Type qs cxt t
    (ns', ret) <- localLambda ts' (generate' t')
    case ret of
        Nothing -> return Nothing
        Just e  -> return (Just (LamE (reverse ns') e))

generate' t = replicateM 2 p >>= return . listToMaybe . catMaybes
  where
    p = do
        m <- randomMatching t
        case m of
            Nothing -> return Nothing
            Just (i, (n, Type qs cxt st), ms) -> do
                (_,_,ctxs,_,s) <- get
                decUses i
                modify (& _5 %~ (`M.union` maybe emptySubst id ms))
                case st of
                    FunT (_:ts) -> do
                        let go [] args        = return (True, args)
                            go (t':ts') args = do
                                ret <- generate' (Type qs cxt t')
                                case ret of
                                    Nothing -> return (False, args)
                                    Just a  -> go ts' (a:args)

                        (success, args) <- go ts []
                        case success of
                            False -> do
                                modify (& _3 .~ ctxs)
                                modify (& _5 .~ s)
                                return Nothing
                            True  -> do
                                let e' = foldl AppE (ConE n) args
                                return (Just e')
                    _ -> return (Just (ConE n))

randomMatching :: Type -> ExpGen (Maybe (Id, Constructor, Maybe Substitution))
randomMatching t = do
    s <- (^. _5) <$> get
    let t' = apply s t

    -- TODO: Filter constraints either in getMatching or by retrying
    -- the random selection until a valid constructor is found (while
    -- keeping track of which ones was tried).
    matches <- getMatching t'
    case length matches of
        0 -> return Nothing
        n -> Just . (matches !!) <$> getRandomR (0, n-1)

getMatching :: Type -> ExpGen [(Id, Constructor, Maybe Substitution)]
getMatching t = concatMap (matchInContext t) <$> getContexts

uniqueTypes :: Constructor -> ExpGen Constructor
uniqueTypes (n, t) = do
    td <- (^. _2) <$> get
    let Type qs cxt st = t
        s' = toSubst (zip (map getName qs) (map ((qs,) . VarT . getName) qs'))
        getCtr (Forall _) = Forall
        getCtr (Exists _) = Exists
        qs' = [ getCtr q (appendName ("_" ++ show i) (getName q))
              | (i, q) <- zip [td..] qs
              ]
        t' = Type qs' (apply s' cxt) (apply s' st)

    modify (& _2 %~ (+ length qs))
    return (n, t')

runEG :: ExpGen a -> Seed -> Language -> (a, EGState)
-- TODO: The environment is not used yet! Need to add this to the
-- EGState.
runEG g seed (L _env cs) = runState g' (0, 0, [], gen, M.empty)
  where
    g'  = unEG $ pushContext cs >> g
    gen = snd . next . mkStdGen $ seed

-- | Pushes a list of constructors to the context stack. Returns the
-- new depth and the number of constructors added.
pushContext :: [Constructor] -> ExpGen (Depth, Int)
pushContext cs = do
    cs' <- mapM uniqueTypes cs
    (depth, td, ctxs, g, s) <- get
    let uses = 10 -- FIXME: arbitrarily chosen
        ctx = M.fromList [ (i, (Just uses, c))
                         | (i, c) <- zip [depth..] cs'
                         ] :: Context
        len = M.size ctx
        depth' = depth + len
    put (depth', td, ctx : ctxs, g, s)
    return (depth', len)

popContext :: ExpGen ()
popContext = modify (& _3 %~ tail)

nextLambda :: ExpGen NextLambda
nextLambda = fmap (^. _1) get

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
    n <- nextLambda
    let cs = map constr (zip [n..] ts)
        ns = map fst cs

    pushContext cs
    a <- eg
    popContext

    return (ns, a)
  where
    -- FIXME: might capture variable names
    constr (i, t) = let (n, c) = i `divMod` 26
                    in (mkName ("_lam_" ++ chr (c + ord 'a') : '_' : show n), t)

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
    go []     = []
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
