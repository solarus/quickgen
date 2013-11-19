{-# LANGUAGE TemplateHaskell, MagicHash #-}

module Testing.QuickGen.TH
       ( unsafeCoerce#
       , p
       ) where

import Control.Monad (liftM)
import GHC.Exts (unsafeCoerce#)
import Language.Haskell.TH as TH

unPlainTV (PlainTV v) = v

nameToNameStr :: (Name -> String) -> Name -> ExpQ
nameToNameStr shw name = return $ LitE (StringL (shw name))

showTypeName = nameBase

showVarName :: Name -> String
showVarName name
    | name == '[]  = "[]" -- data constructor
    | name == ''[] = "[]" -- type constructor
    | otherwise    = show name

expToExpExp :: Exp -> ExpQ
expToExpExp (VarE name)                    = [| VarE (mkName $(nameToNameStr showVarName name)) |]
expToExpExp (ConE name)                    = [| ConE (mkName $(nameToNameStr showVarName name)) |]
expToExpExp (AppE e0 e1)                   = [| AppE $(expToExpExp e0) $(expToExpExp e1) |]
expToExpExp (LamE ps e)                    = [| LamE $(liftM ListE $ mapM patToExpPat ps) $(expToExpExp e) |]
expToExpExp (InfixE Nothing   e Nothing)   = [| InfixE Nothing                  $(expToExpExp e) Nothing |]
expToExpExp (InfixE (Just e0) e Nothing)   = [| InfixE (Just $(expToExpExp e0)) $(expToExpExp e) Nothing |]
expToExpExp (InfixE Nothing   e (Just e1)) = [| InfixE Nothing                  $(expToExpExp e) (Just $(expToExpExp e1)) |]
expToExpExp (InfixE (Just e0) e (Just e1)) = [| InfixE (Just $(expToExpExp e0)) $(expToExpExp e) (Just $(expToExpExp e1)) |]
expToExpExp (TupE es)                      = [| TupE $((return . ListE) =<< mapM expToExpExp es) |]
expToExpExp (CondE e0 e1 e2)               = [| CondE $(expToExpExp e0) $(expToExpExp e1) $(expToExpExp e2) |]
expToExpExp (ListE es)                     = [| ListE $((return . ListE) =<< mapM expToExpExp es) |]
expToExpExp e@(LitE (CharL c))             = [| LitE (CharL     $(return e)) |]
expToExpExp e@(LitE (StringL s))           = [| LitE (StringL   $(return e)) |]
expToExpExp e@(LitE (IntegerL c))          = [| LitE (IntegerL  $(return e)) |]
expToExpExp e@(LitE (RationalL s))         = [| LitE (RationalL $(return e)) |]
expToExpExp (SigE e t)                     = [| SigE $(expToExpExp e) $(typeToExpType t) |]
expToExpExp e                              = [| VarE (mkName $(return $ LitE (StringL (show e)))) |]

-- TODO: documentation
typeToExpType :: Type -> ExpQ
typeToExpType (ForallT ns [] t) =
    [| ForallT (map (PlainTV . mkName)
                    $(return $ ListE $ map (LitE . StringL . showTypeName . unPlainTV) ns))
               []
               $(typeToExpType t) |]
typeToExpType (ForallT _ (_:_) _) = error "typeToExpType: Type classes are not implemented yet."
typeToExpType (ConT name)         = [| ConT (mkName $(nameToNameStr showTypeName name)) |]
typeToExpType (VarT name)         = [| VarT (mkName $(nameToNameStr showTypeName name)) |]
typeToExpType (AppT t0 t1)        = [| AppT $(typeToExpType t0) $(typeToExpType t1) |]
typeToExpType (TupleT n)          = [| TupleT $(return $ LitE (IntegerL (toInteger n))) |]
typeToExpType ArrowT              = [| ArrowT |]
typeToExpType ListT               = [| ListT |]

-- TODO: documentation
patToExpPat (VarP name)          = [| VarP (mkName $(nameToNameStr showVarName name)) |]
patToExpPat (TupP ps)            = [| TupP $(liftM ListE $ mapM patToExpPat ps) |]
patToExpPat (ConP name ps)       = [| ConP (mkName $(nameToNameStr showVarName name)) $(liftM ListE $ mapM patToExpPat ps) |]
patToExpPat (InfixP p0 name p1)  = [| InfixP $(patToExpPat p0) (mkName $(nameToNameStr showVarName name)) $(patToExpPat p1) |]
patToExpPat (TildeP p)           = [| TildeP $(patToExpPat p) |]
patToExpPat (AsP name p)         = [| AsP (mkName $(nameToNameStr showVarName name)) $(patToExpPat p) |]
patToExpPat WildP                = [| WildP |]
patToExpPat (ListP ps)           = [| ListP $(liftM ListE $ mapM patToExpPat ps) |]
patToExpPat (SigP p t)           = [| SigP $(patToExpPat p) $(typeToExpType t) |]
patToExpPat (LitP (IntegerL i))  = [| LitP (IntegerL $(return $ LitE (IntegerL i))) |]
patToExpPat (LitP (CharL c))     = [| LitP (CharL    $(return $ LitE (CharL    c))) |]
patToExpPat (LitP (StringL cs))  = [| LitP (StringL  $(return $ LitE (StringL cs))) |]
patToExpPat (LitP (RationalL r)) = [| LitP (RationalL $(return $ LitE (RationalL r))) |]

-- | 'p' is used to convert your primitive component set into the internal form.
p :: TH.ExpQ -- ^ Quote a tuple of primitive components here.
  -> TH.ExpQ -- ^ This becomes @[Primitive]@ when spliced.
p eq = eq >>= f
  where
    f (TupE es) = return . ListE =<< mapM p1 es

    -- This default pattern should also be defined, because it takes two (or more) to tuple!
    f e         = return . ListE . return =<< p1 e

p1 :: TH.Exp -> TH.ExpQ
p1 se@(SigE e ty) = p1' se e ty
p1 e@(ConE name)  = do
    DataConI _ ty _ _ <- reify name
    p1' e e ty

p1 e@(VarE name)  = do
    VarI _ ty _ _ <- reify name
    p1' e e ty

p1 e              = do
    ee <- expToExpExp e
    return $ TupE [ AppE (ConE (mkName "HV")) (AppE (VarE (mkName "unsafeCoerce#")) e)
                  ,  ee
                  , AppE (VarE (mkName "trToTHType")) (AppE (VarE (mkName "typeOf")) e)
                  ]

p1' se e ty =  do
    ee <- expToExpExp e
    et <- typeToExpType ty
    return $ TupE [ AppE (ConE (mkName "HV")) (AppE (VarE (mkName "unsafeCoerce#")) se)
                  , ee
                  , et
                  ]
