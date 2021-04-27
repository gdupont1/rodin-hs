------------------------------------------------------------------------
-- File: Rodin/Theory/Ascii.hs - Part of rodinapi
------------------------------------------------------------------------
-- Copyright (C) 2019  G. Dupont
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
------------------------------------------------------------------------
{-|
Module      : Rodin.Theory.Ascii
Description : ASCII exporter for Rodin theories
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

Instances of 'Rodin.Ascii' for 'Rodin.Theory.Theory' types.
-}
module Rodin.Theory.Ascii where

import Rodin.Internal.Util
import Rodin.Ascii
import Rodin.Formula.Tokenizer
import Rodin.Formula.Ascii
import Rodin.Theory

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.NotationType'
instance ShowAscii NotationType where
  showAscii Prefix = ""
  showAscii Infix = "INFIX "

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.OperatorProp'
instance ShowAscii OperatorProp where
  showAscii op =
      (if as then " associative" else "") ++
      (if co then " commutative" else "") ++
      (if fo then " expression" else " predicate") ++
      " " ++ showAscii no
      where as = associative  op
            co = commutative  op
            fo = formulaType  op
            no = notationType op

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.OperatorArgument'
instance ShowAscii OperatorArgument where
  showAscii oa =
      id ++ ": " ++ printAscii ex
      where id = identifier oa
            ex = expression oa

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.OperatorWDCondition'
instance ShowAscii OperatorWDCondition where
  showAscii = printAscii . predicate

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.OperatorDirectDefinition'
instance ShowAscii OperatorDirectDefinition where
  showAscii odd =
      "\n" ++ ind 3 ++ "direct definition " ++ printAsciiLines 4 fo
      where fo = formula odd

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.RecursiveDefinitionCase'
instance ShowAscii RecursiveDefinitionCase where
  showAscii rdc =
      "\n" ++ ind 4 ++ printAscii ex ++ " => " ++ printAscii fo
      where ex = caseExpression rdc
            fo = caseFormula    rdc

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.OperatorRecursiveDefinition'
instance ShowAscii OperatorRecursiveDefinition where
  showAscii ord =
      "\n" ++ ind 3 ++ "case " ++ ia ++ ":\n" ++ (asciilist "" ca)
      where ia = inductiveArgument ord
            ca = cases             ord

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.ImportTheory'
instance ShowAscii ImportTheory where
  showAscii = theory

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.ImportTheoryProject'
instance ShowAscii ImportTheoryProject where
  showAscii itp =
      "\n" ++ ind 2 ++ pr ++ " THEORIES " ++ (asciilist "," th)
      where pr = project  itp
            th = theories itp

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.TypeParameter'
instance ShowAscii TypeParameter where
  showAscii = typeIdentifier

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.ConstructorArgument'
instance ShowAscii ConstructorArgument where
  showAscii ca =
      id ++ ":" ++ printAscii ty
      where id = caId   ca
            ty = caType ca

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.DataTypeConstructor'
instance ShowAscii DataTypeConstructor where
  showAscii dtc =
      "\n" ++ ind 3 ++ id ++ "(" ++ (asciilist "," ar) ++ ")"
      where id = dtcId dtc
            ar = args  dtc

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.TypeArgument'
instance ShowAscii TypeArgument where
  showAscii = printAscii . typeArg

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.DataTypeDefinition'
instance ShowAscii DataTypeDefinition where
  showAscii dtd =
      "\n" ++ ind 2 ++ id ++ (if not $ null ar then "(" ++ asciilist "," ar ++ ")" else "") ++
      (if not $ null co then "\n" ++ ind 2 ++ "CONSTRUCTORS" ++ asciilist "" co else "")
      where id = dtId          dtd
            ar = typeArguments dtd
            co = constructors  dtd

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.NewOperatorDefinition'
instance ShowAscii NewOperatorDefinition where
  showAscii nod =
      "\n" ++ ind 2 ++ la ++ showAscii pr ++ "(" ++ (asciilist "," ar) ++ ")" ++
      (if not $ null wd then "\n" ++ ind 3 ++ "well-definedness " ++ asciilist "," wd else "") ++
      (if not $ null di then asciilist "" di else "") ++
      (if not $ null re then asciilist "" re else "")
      where la = opLabel  nod
            pr = opProp   nod
            ar = opArgs   nod
            wd = opWD     nod
            di = opDirDef nod
            re = opRecDef nod

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.AxiomaticTypeDefinition'
instance ShowAscii AxiomaticTypeDefinition where
  showAscii = aTypeId


-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.AxiomaticOperatorDefinition'
instance ShowAscii AxiomaticOperatorDefinition where
  showAscii aod =
      "\n" ++ ind 3 ++ la ++ showAscii pr ++ "(" ++ (asciilist "," ar) ++ ") : " ++ printAscii ty ++
      (if not $ null wd then "\n" ++ ind 4 ++ "well-definedness " ++ (asciilist "," wd) else "")
      where la = aOpLabel aod
            pr = aOpProp  aod
            ty = aType    aod
            ar = aOpArgs  aod
            wd = aOpWD    aod

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.AxiomaticDefinitionAxiom'
instance ShowAscii AxiomaticDefinitionAxiom where
  showAscii ad =
      "\n" ++ ind 3 ++ la ++ ": " ++ printAsciiLines 4 pr
      where la = aDefLabel     ad
            pr = aDefPredicate ad

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.AxiomaticDefinitionsBlock'
instance ShowAscii AxiomaticDefinitionsBlock where
  showAscii adb =
      ind 1 ++ la ++ ":" ++
      (if not $ null ty then "\n" ++ ind 2 ++ "TYPES " ++ (asciilist ", " ty) else "") ++
      (if not $ null de then "\n" ++ ind 2 ++ "OPERATORS" ++ (asciilist "" de) else "") ++
      (if not $ null ax then "\n" ++ ind 2 ++ "AXIOMS" ++ (asciilist "" ax) else "")
      where la = aDefBLabel adb
            ty = aDefBTypes adb
            de = aDefBDef   adb
            ax = aDefBAx    adb


-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.Theorem'
instance ShowAscii Theorem where
  showAscii th =
      "\n" ++ ind 2 ++ na ++ ": " ++ printAsciiLines 3 pr
      where na = thName      th
            pr = thPredicate th

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.MetaVariable'
instance ShowAscii MetaVariable where
  showAscii mv =
      "\n" ++ ind 3 ++ id ++ ": " ++ printAscii ty
      where id = mvId   mv
            ty = mvType mv

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.InferenceGiven'
instance ShowAscii InferenceGiven where
  showAscii = printAscii . givenPredicate

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.InferenceInfer'
instance ShowAscii InferenceInfer where
  showAscii = printAscii . inferPredicate

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.InferenceRule'
instance ShowAscii InferenceRule where
  showAscii ir =
      "\n" ++ ind 3 ++ la ++ ": " ++ (asciilist "," gi) ++ " |- " ++ (asciilist "," ie)
      where la = infLabel ir
            ap = infApp   ir
            de = infDesc  ir
            gi = given    ir
            ie = infer    ir

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.RewriteRuleRHS'
instance ShowAscii RewriteRuleRHS where
  showAscii rr =
      "\n" ++ ind 4 ++ la ++ ": " ++ printAscii pr ++ " => " ++ printAscii fo
      where la = rhsLabel     rr
            pr = rhsPredicate rr
            fo = rhsFormula   rr

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.RewriteRule'
instance ShowAscii RewriteRule where
  showAscii rr =
      "\n" ++ ind 3 ++ la ++ ": " ++ printAscii ls ++ (asciilist "" rs)
      where la = rewLabel rr
            ap = rewApp   rr
            co = complete rr
            de = rewDesc  rr
            ls = lhs      rr
            rs = rhs      rr

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.ProofRulesBlock'
instance ShowAscii ProofRulesBlock where
  showAscii prb =
      "\n" ++ ind 2 ++ la ++ ":" ++
      (if not $ null mv then "\n" ++ ind 2 ++ "Metavariables" ++ (asciilist "" mv) else "") ++
      (if not $ null rr then "\n" ++ ind 2 ++ "Rewrite Rules" ++ (asciilist "" rr) else "") ++
      (if not $ null ir then "\n" ++ ind 2 ++ "Inference Rules" ++ (asciilist "" ir) else "")
      where la = prBLabel       prb
            mv = metaVariables  prb
            ir = inferenceRules prb
            rr = rewriteRules   prb

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Theory.Theory'
instance ShowAscii Theory where
  showAscii thy =
      "THEORY " ++ tn ++
      (if not $ null im then "\n" ++ ind 1 ++ "IMPORT THEORY PROJECTS" ++ (asciilist "" im) else "") ++
      (if not $ null ty then "\n" ++ ind 1 ++ "TYPE PARAMETERS "       ++ (asciilist "," ty) else "") ++
      (if not $ null da then "\n" ++ ind 1 ++ "DATA TYPES"             ++ (asciilist "" da) else "") ++
      (if not $ null op then "\n" ++ ind 1 ++ "OPERATORS"              ++ (asciilist "" op) else "") ++
      (if not $ null ax then "\n" ++ ind 1 ++ "AXIOMATIC DEFINITIONS"  ++ (asciilist "" ax) else "") ++
      (if not $ null th then "\n" ++ ind 1 ++ "THEOREMS"               ++ (asciilist "" th) else "") ++
      (if not $ null pr then "\n" ++ ind 1 ++ "PROOF RULES"            ++ (asciilist "" pr) else "") ++
      "\nEND"
      where tn = theoryName           thy
            im = imports              thy
            ty = typeParameters       thy
            da = datatypeDefinitions  thy
            op = operators            thy
            ax = axiomaticDefinitions thy
            th = theorems             thy
            pr = proofRules           thy





