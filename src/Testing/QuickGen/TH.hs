{-# LANGUAGE TemplateHaskell #-}

module Testing.QuickGen.TH
       ( constructors
       , printTH
       ) where

import           Data.Functor ((<$>))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.Exts as Exts ()

-- For debugging splices function
printTH e = TH.runQ e >>= print

-- | Parses a list of constructors.
constructors e = do
    TH.TupE es <- e
    return . TH.ListE =<< mapM f es
  where
    f e@(TH.VarE name) = do
        ret <- TH.reify name
        t' <- liftType $ case ret of
            TH.VarI _ t _ _     -> t
            TH.ClassOpI _ t _ _ -> t
        e' <- liftExp e
        return (TH.TupE [e', t'])
    f (TH.SigE e@(TH.VarE name) t) = do
        e' <- liftExp e
        t' <- liftType t
        return (TH.TupE [e', t'])

-- | Lifts a TH.Exp value to its corresponding Exp value for use in
-- splices.
liftExp :: TH.Exp -> TH.ExpQ
-- Only implement identifiers for now
liftExp (TH.VarE name) = [| TH.VarE (TH.mkName $(nameToExp name)) |]
liftExp _              = error "TH.liftExp: Only regular identifiers are \
                               \implemented as of now."

-- | Lifts a TH.Type value to its corresponding Exp value.
liftType :: TH.Type -> TH.ExpQ
liftType (TH.ForallT ns cxt t) =
    [| TH.ForallT
               (map (TH.PlainTV . TH.mkName) $(return . TH.ListE . map liftTyVarBndr $ ns))
               $(TH.ListE <$> (mapM liftPred cxt))
               $(liftType t) |]
liftType (TH.ConT name)         = [| TH.ConT (TH.mkName $(nameToExp name)) |]
liftType (TH.VarT name)         = [| TH.VarT (TH.mkName $(nameToExp name)) |]
liftType (TH.AppT t1 t2)        = [| TH.AppT $(liftType t1) $(liftType t2) |]
liftType (TH.TupleT n)          = [| TH.TupleT $(return $ TH.LitE (TH.IntegerL (toInteger n))) |]
liftType TH.ArrowT              = [| TH.ArrowT |]
liftType TH.ListT               = [| TH.ListT |]

-- | Lifts a TH.Pred value to its corresponding Exp value.
liftPred :: TH.Pred -> TH.ExpQ
liftPred (TH.ClassP n ts)  = [| TH.ClassP (TH.mkName $(nameToExp n)) $(return . TH.ListE =<< mapM liftType ts) |]
liftPred (TH.EqualP t1 t2) = [| TH.EqualP $(liftType t1) $(liftType t2) |]

-- | Lifts a TH.TyVarBndr value to its corresponding Exp value.
liftTyVarBndr :: TH.TyVarBndr -> TH.Exp
liftTyVarBndr (TH.PlainTV v) = TH.LitE . TH.StringL . TH.nameBase $ v

-- | Lifts a TH.Name value to its corresponding Exp value.
nameToExp :: TH.Name -> TH.ExpQ
nameToExp name = return . TH.LitE . TH.StringL . TH.nameBase $ name
