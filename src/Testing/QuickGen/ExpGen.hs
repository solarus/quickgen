{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving
           , TupleSections #-}

module Testing.QuickGen.ExpGen
       ( ExpGen
       , generate
       , match
       ) where

import           Control.Applicative
import           Control.Lens (Field2, (^.), (&), (%~), (.~), _1, _2, _3, _5)
import           Control.Monad.State
import           Data.Char (chr, ord)
import           Data.List ((\\))
import qualified Data.Map as M
import           Data.Maybe (catMaybes, listToMaybe)
import           System.Random

import           Testing.QuickGen.Types

type NextLambda = Nat
type NextType   = Nat

-- TODO: Make this a data type and derive lenses instead of using i.e.
-- _1 or pattern matching and the whole state when using get.
type EGState = (NextLambda, NextType, [Context], StdGen, Substitution)

newtype ExpGen a = EG { unEG :: State EGState a }
  deriving (Functor, Monad, Applicative, MonadState EGState)

generate :: Language -> Type -> Seed -> (Maybe Exp, EGState)
generate ctx t seed = runEG seed ctx $ do
    t' <- bindForall <$> uniqueTypes t
    generate' t'

generate' :: Type -> ExpGen (Maybe Exp)
generate' (Type qs cxt (FunT (t:ts))) = do
    let -- TODO: clean up qs and cxt?
        ts' = map (Type qs cxt) ts
        t'  = Type qs cxt t
    (ns', ret) <- localLambda ts' (generate' t')
    return (LamE (reverse ns') <$> ret)

generate' t = replicateM 2 p >>= return . listToMaybe . catMaybes
  where
    p = do
        m <- randomMatching t
        case m of
            Nothing -> return Nothing
            Just (i, (n, Type qs cxt st), s) -> do
                (_,_,ctxs,_,s') <- get
                modify (& _5 %~ maybe (error "should not happen") id . (`unionSubst` s))
                decUses i
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
                                modify (& _5 .~ s')
                                return Nothing
                            True  -> do
                                let e' = foldl AppE (ConE n) args
                                return (Just e')
                    _ -> return (Just (ConE n))

randomMatching :: Type -> ExpGen (Maybe (Id, Constructor, Substitution))
randomMatching goalType = do
    subst <- getSubstitution
    let gt = applys subst goalType
        f i (mu, (n, t)) acc
            -- If number of uses is a Just and less than 1 then
            -- discard this constructor. TODO: maybe remove
            -- constructor from the context instead when decreasing
            -- uses?
            | maybe False (< 1) mu = acc
            | otherwise            = do
                t' <- uniqueTypes (applys subst t)
                case runStateT (match gt t') emptySubst of
                    Just (newT, s) -> (:) <$> pure (i, (n, newT), s) <*> acc
                    Nothing -> acc

    ctxs    <- getContexts
    matches <- fmap concat . forM ctxs $ foldrContext f (return [])

    case matches of
        [] -> return Nothing
        _  -> Just . (matches !!) <$> getRandomR (0, length matches - 1)

-- | Given a type replaces all `Forall' bound variables in that type
-- with unique type variables. Updates the EGState with the next free
-- type variable id.
uniqueTypes :: Type -> ExpGen Type
uniqueTypes t@(Type vs _ _) = do
    td <- (^. _2) <$> get
    let subst = toSubst [ (n, let v = (i, Forall) in ([v], VarT v))
                        | (i, (n, Forall)) <- zip [td..] vs
                        ]
    modify (& _2 %~ (+ sizeSubst subst))
    return (apply subst t)

runEG :: Seed -> Language -> ExpGen a -> (a, EGState)
-- TODO: The environment is not used yet! Need to add this to the
-- EGState.
runEG seed (L _env cs) g = runState g' (0, 0, [], gen, M.empty)
  where
    g'  = unEG $ pushContext cs >> g
    gen = snd . next . mkStdGen $ seed

-- | Pushes a list of constructors to the context stack. Returns the
-- new depth and the number of constructors added.
pushContext :: [Constructor] -> ExpGen (Depth, Int)
pushContext cs = do
    (depth, td, ctxs, g, s) <- get
    let uses = 3 -- FIXME: arbitrarily chosen
        ctx = M.fromList [ (i, (Just uses, c))
                         | (i, c) <- zip [depth..] cs
                         ] :: Context
        len = M.size ctx
        depth' = depth + len
    modify ((_3 %~ (ctx:)) . (_1 .~ depth'))
    return (depth', len)

popContext :: ExpGen ()
popContext = modify (& _3 %~ tail)

nextLambda :: ExpGen NextLambda
nextLambda = fmap (^. _1) get

getContexts :: ExpGen [Context]
getContexts = (^. _3) <$> get

getSubstitution :: ExpGen Substitution
getSubstitution = (^. _5) <$> get

getRandomR :: (Int, Int) -> ExpGen Int
getRandomR p = state f
  where
    f (d, td, cs, g, s) = let (a, g') = randomR p g in (a, (d, td, cs, g', s))

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

findAndUpdate :: ((Uses, Constructor) -> (Uses, Constructor)) -> Id -> [Context] -> [Context]
findAndUpdate f i = go
  where
    f' _ a = Just (f a)
    go []     = []
    go (c:cs) = case M.updateLookupWithKey f' i c of
        (Nothing, _) -> c : go cs
        (Just _, c') -> c' : cs

match :: (Applicative m, Monad m) => Type -> Type -> StateT Substitution m Type
match t1@(Type ns1 _ _) t2 = do
    s <- match' t1 t2

    -- FIXME: I'm basically doing exactly what forallToUndecided does
    -- here but for a Type instead of a SType. Need to generalize.
    let t2'@(Type ns2 _ _) = applys s t2
        subst   = toSubst [ (n, let n' = (n, Undecided) in ([n'], VarT n'))
                          | (n, Forall) <- ns2
                          ]

    return (applys subst t2')

match' :: Monad m => Type -> Type -> StateT Substitution m Substitution
match' t1@(Type _ _ (FunT _)) _ = error $ "match: Unexpected function type " ++ show t1
match' t1 (Type ns cxt (FunT (t2 : _))) = match' t1 (Type ns cxt t2)
match' ta@(Type _ _ st1) tb@(Type _ _ st2) = go st1 st2
  where
    go :: Monad m => SType -> SType -> StateT Substitution m Substitution
    go t1@(VarT (_, Forall)) t2 = error $ "t1 = " ++ show t1 ++ " | t2 = " ++ show t2
    go t1@(VarT (n1, Undecided)) (VarT (n2, Undecided))
        | n1 == n2 = return emptySubst
    go t1 (VarT (n2, Forall)) = return (n2 |-> t1)
    go t1 (VarT v@(n2, Undecided))
        | v `elem` getVars t1 = fail "No match"
        | otherwise = get >>= \s -> case lookupSubst n2 s of
        Just (_, t2') -> go t1 t2'
        _             -> do
            let ret = (n2 |-> t1)
            case unionSubst s ret of
                Just s' -> put s'
                Nothing -> fail "No match"
            return ret
    go (VarT v@(n1, Undecided)) t2
        | v `elem` (getVars t2) = fail "No match"
        | otherwise = get >>= \s -> case lookupSubst n1 s of
        Just (_, t1') -> go t1' t2
        _             -> do
            let t2' = forallToUndecided t2

            case unionSubst s (n1 |-> t2') of
                Just s' -> put s'
                Nothing -> fail "No match"
            return emptySubst

    go (ListT t1) (ListT t2) = go t1 t2
    go (ListT _)  _          = noMatch
    go _          (ListT _)  = noMatch

    go (ConT n1 as1) (ConT n2 as2)
        | n1 /= n2  = noMatch
        | otherwise = unionsSubst =<< zipWithM go as1 as2
    go (ConT _ _) _ = noMatch
    go _ (ConT _ _) = noMatch

    go _ _ = error $ "match: Not matched " ++ show ta ++ " | " ++ show tb

    noMatch :: Monad m => m a
    noMatch = fail $ "Types don't match: " ++ show ta ++ " | " ++ show tb

-- TODO: Remove need for this by doing something smarter in
-- substitution.
applys :: Substitution -> Type -> Type
applys s t = t'
  where
    ts = iterate (apply s) t
    t' = fst . head . dropWhile (uncurry (/=)) $ zip ts (tail ts)
