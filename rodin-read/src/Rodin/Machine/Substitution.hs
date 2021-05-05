{-|
Module      : Rodin.Machine.Substitution
Description : @Rodin.Substitution.Substituable@ instances for @Rodin.Machine@
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com
-}
module Rodin.Machine.Substitution where

import Rodin.Formula.Substitution
import Rodin.Machine
import Rodin.Substitution

instance Substituable RefinesMachine 
instance Substituable SeesContext

instance Substituable Variable where
  substitute st var =
      var { vaIdentifier = substitute st $ vaIdentifier var }

instance Substituable Invariant where
  substitute st inv =
      inv { invPred = substitute st $ invPred inv }

instance Substituable Variant where
  substitute st var = 
      var { varExpression = substitute st $ varExpression var }

instance Substituable RefinesEvent

instance Substituable Parameter where
  substitute st par =
      par { paIdentifier = substitute st $ paIdentifier par }

instance Substituable Guard where
  substitute st grd =
      grd { guPred = substitute st $ guPred grd }

instance Substituable Witness where
  substitute st wit = 
      wit { wiPred = substitute st $ wiPred wit
          , wiLabel = substitute st $ wiLabel wit }

instance Substituable Action where
  substitute st act =
      act { acAssignment = substitute st $ acAssignment act }

instance Substituable Event where
  substitute st evt =
      evt { evParameters = substituteAll st $ evParameters evt
          , evGuards     = substituteAll st $ evGuards evt
          , evWitnesses  = substituteAll st $ evWitnesses evt
          , evActions    = substituteAll st $ evActions evt }

instance Substituable Machine where
  substitute st mac = 
      mac { maVariables  = substituteAll st $ maVariables mac
          , maInvariants = substituteAll st $ maInvariants mac
          , maVariants   = substituteAll st $ maVariants mac
          , maEvents     = substituteAll st $ maEvents mac }



