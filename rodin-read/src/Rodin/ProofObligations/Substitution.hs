{-|
Module      : Rodin.ProofObligations.Substitution
Description : @Rodin.Substitution.Substituable@ instances for @Rodin.ProofObligations@
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com
-}
module Rodin.ProofObligations.Substitution where

import Rodin.Formula.Substitution
import Rodin.Substitution
import Rodin.ProofObligations

instance Substituable POIdentifier where
  substitute st poi = 
      poi { poiType = substitute st $ poiType poi }

instance Substituable POPredicate where
  substitute st pop = 
      pop { popPredicate = substitute st $ popPredicate pop }

instance Substituable POPredicateSet where
  substitute st pops = 
      pops { popsIdentifiers = substituteAll st $ popsIdentifiers pops
           , popsPredicates  = substituteAll st $ popsPredicates  pops }

instance Substituable POSource 
instance Substituable POSelHint 

instance Substituable POSequent where
  substitute st pos = 
      pos { posPredicateSets = substituteAll st $ posPredicateSets pos
          , posPredicates    = substituteAll st $ posPredicates    pos }

instance Substituable POFile where
  substitute st pof = 
      pof { pofPredicateSets = substituteAll st $ pofPredicateSets pof
          , pofSequents      = substituteAll st $ pofSequents      pof }




