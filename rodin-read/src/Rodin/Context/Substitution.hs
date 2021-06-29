{-|
Module      : Rodin.Context.Substitution
Description : @Rodin.Substitution.Substituable@ instances for @Rodin.Context@
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com
-}
module Rodin.Context.Substitution where

import Rodin.Formula.Substitution
import Rodin.Context
import Rodin.Substitution

instance Substituable ExtendsContext

instance Substituable Axiom where
  substitute st axm =
      axm { axPred = substitute st $ axPred axm }

instance Substituable Theorem where
  substitute st thm =
      thm { thPred = substitute st $ thPred thm }

instance Substituable Constant
instance Substituable CarrierSet

instance Substituable Context where
  substitute st ctx = 
      ctx { ctxAxioms   = substituteAll st $ ctxAxioms   ctx,
            ctxTheorems = substituteAll st $ ctxTheorems ctx }


