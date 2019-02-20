{-# LANGUAGE CPP #-}
module TemplateHaskell.Compat.V0208 where

import BasePrelude
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

dataD :: Cxt -> Name -> [TyVarBndr] -> [Con] -> [Name] -> Dec
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
