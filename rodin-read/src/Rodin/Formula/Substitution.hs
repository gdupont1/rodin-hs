{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module      : Rodin.Formula.Substitution
Description : @Rodin.Substitution.Substituable@ instances for @Rodin.Formula@
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com
-}
module Rodin.Formula.Substitution where

import Rodin.Formula
import Rodin.Substitution

-- | Note that `Formula = [Token]`
instance Substituable [Token] where
  substitute = substituteFormula 


