{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE GeneralizedNewtypeDeriving
           , TupleSections
           , NamedFieldPuns #-}

module Testing.QuickGen.ExpGen
       ( ExpGen
       , EGState
       , generate
       , match
       ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.Char (chr, ord)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, listToMaybe)
import           System.Random

import           Testing.QuickGen.Types

type NextLambda = Nat
type NextType   = Nat

-- TODO: Make this a data type and derive lenses instead of using i.e.
-- _1 or pattern matching and the whole state when using get.
data EGState = S { nextLambda :: NextLambda
                 , nextType   :: NextType
                 , contexts   :: [Context]
                 , stdGen     :: StdGen
                 , guesses    :: Substitution
                 , classEnv   :: ClassEnv
                 }

newtype ExpGen a = EG { unEG :: State EGState a }
  deriving (Functor, Monad, Applicative, MonadState EGState)

generate :: Language -> Type -> Seed -> Maybe (Exp, Type)
generate lang t seed = (, applys guesses t) <$> e
  where
    (e, S { guesses }) = runEG seed lang $ do
        t' <- bindForall <$> uniqueTypes t
        generate' t'


generate' :: Type -> ExpGen (Maybe Exp)
generate' (Type qs cxt (FunT (t:ts))) = do
    let -- TODO: clean up qs and cxt?
        ts' = map (Type qs cxt) ts
        t'  = Type qs cxt t
    (ns', ret) <- localLambda ts' (generate' t')
    return (LamE (reverse ns') <$> ret)

generate' t = replicateM 3 p >>= return . listToMaybe . catMaybes
  where
    p = do
        m <- randomMatching t
        case m of
            Nothing -> return Nothing
            Just (i, (n, Type qs cxt st), s) -> do
                S { contexts, guesses } <- get
                let u' = maybe (error "should not happen") id (guesses `unionSubst` s)
                modify (\s -> s { guesses = u' })
                decUses i
                case st of
                    FunT (_:ts) -> do
                        let go []       args = return . Just $ foldl AppE (ConE n) args
                            go (t':ts') args = do
                                ret <- generate' (Type qs cxt t')
                                case ret of
                                    Nothing -> do
                                        modify (\s -> s { contexts = contexts
                                                        , guesses = guesses })
                                        return Nothing
                                    Just a -> go ts' (a:args)

                        go ts []
                    _ -> return (Just (ConE n))

randomMatching :: Type -> ExpGen (Maybe (Id, Constructor, Substitution))
randomMatching goalType = do
    guesses <- getGuesses
    let gt = applys guesses goalType
        f i (mu, (n, t)) acc
            -- If number of uses is a Just and less than 1 then
            -- discard this constructor. TODO: maybe remove
            -- constructor from the context instead when decreasing
            -- uses?
            | maybe False (< 1) mu = acc
            | otherwise            = do
                t' <- uniqueTypes (applys guesses t)
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
    td <- getNextType
    let subst = toSubst [ (n, let v = (i, Forall) in ([v], VarT v))
                        | (i, (n, Forall)) <- zip [td..] vs
                        ]
    modify (\s -> s { nextType = sizeSubst subst + (nextType s) })
    return (apply subst t)

runEG :: Seed -> Language -> ExpGen a -> (a, EGState)
runEG seed (L env cs) g = runState g' (S 0 0 [] gen emptySubst env)
  where
    g'  = unEG $ pushContext cs >> g
    gen = snd . next . mkStdGen $ seed

-- | Pushes a list of constructors to the context stack. Returns the
-- new depth and the number of constructors added.
pushContext :: [Constructor] -> ExpGen (Depth, Int)
pushContext cs = do
    S { nextLambda } <- get
    let uses = 20 -- FIXME: arbitrarily chosen
        getUses (_, t)
            | isSimple t = Nothing
            | otherwise  = Just (max 1 (uses - numArgs t))
        ctx = M.fromList [ (i, (getUses c, c))
                         | (i, c) <- zip [nextLambda..] cs
                         ] :: Context
        len = M.size ctx
        nextLambda' = nextLambda + len
    modify (\s -> s { nextLambda = nextLambda', contexts = ctx : contexts s})
    return (nextLambda', len)

popContext :: ExpGen ()
popContext = modify (\s -> s { contexts = tail (contexts s)})

getNextLambda :: ExpGen NextLambda
getNextLambda = nextLambda <$> get

getNextType :: ExpGen NextType
getNextType = nextType <$> get

getContexts :: ExpGen [Context]
getContexts = contexts <$> get

getGuesses :: ExpGen Substitution
getGuesses = guesses <$> get

getEnv :: ExpGen ClassEnv
getEnv = classEnv <$> get

getRandomR :: (Int, Int) -> ExpGen Int
getRandomR p = do
    g <- stdGen <$> get
    let (a, g') = randomR p g
    modify (\s -> s { stdGen = g' } )
    return a

localLambda :: [Type] -> ExpGen a -> ExpGen ([Name], a)
localLambda ts eg = do
    n <- getNextLambda
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
modContext f = modify (\s -> s { contexts = f (contexts s) })

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
    go t1 (VarT (n2, Forall)) = return (n2 |-> t1)
    go t1@(VarT (n1, Undecided)) (VarT (n2, Undecided))
        | n1 == n2 = return emptySubst
    go t1 (VarT v@(n2, Undecided))
        | v `elem` getVars t1 = fail "No match"
        | otherwise = get >>= \s -> case lookupSubst n2 s of
        Just (_, t2') -> go t1 t2'
        _             -> do
            let ret = (n2 |-> t1)
            case unionSubst s ret of
                Just s' -> put s'
                Nothing -> error "ExpGen.match': The impossible happened!"
            return emptySubst
    go (VarT v@(n1, Undecided)) t2
        | v `elem` (getVars t2) = fail "No match"
        | otherwise = get >>= \s -> case lookupSubst n1 s of
        Just (_, t1') -> go t1' t2
        _             -> do
            let t2' = forallToUndecided t2

            case unionSubst s (n1 |-> t2') of
                Just s' -> put s'
                Nothing -> error "ExpGen.match': The impossible happened!"
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
