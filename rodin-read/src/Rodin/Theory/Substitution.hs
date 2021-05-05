{-|
Module      : Rodin.Theory.Substitution
Description : @Rodin.Substitution.Substituable@ instances for @Rodin.Theory@
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com
-}
module Rodin.Theory.Substitution where

import Rodin.Formula.Substitution
import Rodin.Substitution
import Rodin.Theory

instance Substituable OperatorProp

instance Substituable OperatorArgument where
  substitute st arg = 
      arg { expression = substitute st $ expression arg }

instance Substituable OperatorWDCondition where
  substitute st wdc = 
      wdc { predicate = substitute st $ predicate wdc }

instance Substituable OperatorDirectDefinition where
  substitute st ddf =
      ddf { formula = substitute st $ formula ddf }

instance Substituable RecursiveDefinitionCase where
  substitute st rdc =
      rdc { caseExpression = substitute st $ caseExpression rdc
          , caseFormula    = substitute st $ caseFormula    rdc }

instance Substituable OperatorRecursiveDefinition where
  substitute st ord = 
      ord { cases = substituteAll st $ cases ord }

instance Substituable ImportTheory
instance Substituable ImportTheoryProject
instance Substituable TypeParameter

instance Substituable ConstructorArgument where
  substitute st ca = 
      ca { caType = substitute st $ caType ca }

instance Substituable DataTypeConstructor where
  substitute st dtc =
      dtc { args = substituteAll st $ args dtc }

instance Substituable TypeArgument where
  substitute st ta = 
      ta { typeArg = substitute st $ typeArg ta }

instance Substituable DataTypeDefinition where
  substitute st dtd = 
      dtd { typeArguments = substituteAll st $ typeArguments dtd
          , constructors  = substituteAll st $ constructors  dtd }

instance Substituable NewOperatorDefinition where
  substitute st nod = 
      nod { opArgs   = substituteAll st $ opArgs   nod 
          , opWD     = substituteAll st $ opWD     nod
          , opDirDef = substituteAll st $ opDirDef nod
          , opRecDef = substituteAll st $ opRecDef nod }

instance Substituable AxiomaticTypeDefinition

instance Substituable AxiomaticOperatorDefinition where
  substitute st aod = 
      aod { aType   = substitute    st $ aType   aod
          , aOpArgs = substituteAll st $ aOpArgs aod
          , aOpWD   = substituteAll st $ aOpWD   aod }

instance Substituable AxiomaticDefinitionAxiom where
  substitute st ad =
      ad { aDefPredicate = substitute st $ aDefPredicate ad }

instance Substituable AxiomaticDefinitionsBlock where
  substitute st adb = 
      adb { aDefBTypes = substituteAll st $ aDefBTypes adb
          , aDefBDef   = substituteAll st $ aDefBDef   adb
          , aDefBAx    = substituteAll st $ aDefBAx    adb }

instance Substituable Theorem where
  substitute st thm =
      thm { thPredicate = substitute st $ thPredicate thm }

instance Substituable MetaVariable where
  substitute st ty =
      ty { mvType = substitute st $ mvType ty }

instance Substituable InferenceGiven where
  substitute st ig =
      ig { givenPredicate = substitute st $ givenPredicate ig }

instance Substituable InferenceInfer where
  substitute st ii = 
      ii { inferPredicate = substitute st $ inferPredicate ii }

instance Substituable InferenceRule where
  substitute st ir = 
      ir { given = substituteAll st $ given ir
         , infer = substituteAll st $ infer ir }

instance Substituable RewriteRuleRHS where
  substitute st rhs = 
      rhs { rhsPredicate = substitute st $ rhsPredicate rhs
          , rhsFormula   = substitute st $ rhsFormula   rhs }

instance Substituable RewriteRule where
  substitute st rr = 
      rr { lhs = substitute    st $ lhs rr
         , rhs = substituteAll st $ rhs rr }

instance Substituable ProofRulesBlock where
  substitute st prb = 
      prb { metaVariables  = substituteAll st $ metaVariables  prb
          , inferenceRules = substituteAll st $ inferenceRules prb
          , rewriteRules   = substituteAll st $ rewriteRules   prb }

instance Substituable Theory where
  substitute st th = 
      th { typeParameters       = substituteAll st $ typeParameters       th
         , datatypeDefinitions  = substituteAll st $ datatypeDefinitions  th
         , operators            = substituteAll st $ operators            th
         , axiomaticDefinitions = substituteAll st $ axiomaticDefinitions th
         , theorems             = substituteAll st $ theorems             th
         , proofRules           = substituteAll st $ proofRules           th }





