{-# LANGUAGE CPP #-}
module TemplateHaskell.Compat.V0208 where

import TemplateHaskell.Compat.V0208.Prelude
import Language.Haskell.TH


classP :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
classP n tl =
  foldl AppT (ConT n) tl
#else
classP =
  ClassP
#endif

instanceD :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)  
instanceD =
  InstanceD Nothing
#else
instanceD =
  InstanceD
#endif

dataD :: Cxt -> Name -> [UnitTyVarBndr] -> [Con] -> [Name] -> Dec
dataD cxt name varBndrs cons derivingNames =
#if MIN_VERSION_template_haskell(2,12,0)
  DataD cxt name varBndrs Nothing cons (pure (DerivClause Nothing (map ConT derivingNames)))
#elif MIN_VERSION_template_haskell(2,11,0)
  DataD cxt name varBndrs Nothing cons (map ConT derivingNames)
#else
  DataD cxt name varBndrs cons derivingNames
#endif

notStrict :: Strict
notStrict =
#if MIN_VERSION_template_haskell(2,11,0)
  unsafePerformIO (runQ (bang noSourceUnpackedness noSourceStrictness))
#else
  NotStrict
#endif

tupE :: [Exp] -> Exp
#if MIN_VERSION_template_haskell(2,16,0)
tupE = \ case
  [a] -> a
  a -> TupE (map Just a)
#else
tupE = TupE
#endif

specifiedPlainTV :: Name -> SpecificityTyVarBndr
#if MIN_VERSION_template_haskell(2,17,0)
specifiedPlainTV = flip PlainTV SpecifiedSpec
#else
specifiedPlainTV = PlainTV
#endif

#if MIN_VERSION_template_haskell(2,17,0)
type SpecificityTyVarBndr = TyVarBndr Specificity
#else
type SpecificityTyVarBndr = TyVarBndr
#endif

#if MIN_VERSION_template_haskell(2,17,0)
type UnitTyVarBndr = TyVarBndr ()
#else
type UnitTyVarBndr = TyVarBndr
#endif
