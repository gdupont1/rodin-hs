--------------------------------------------------------------------------
-- File: Rodin/Theory/TeX.hs - Part of rodinapi-tex
--------------------------------------------------------------------------
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
--------------------------------------------------------------------------
{-|
Module      : Rodin.Theory.TeX
Description : Module for printing a Theory to LaTeX
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module mainly contains an instanciation of the 'Rodin.TeX.ShowTeX' for
the 'Rodin.Theory' type.
-}
module Rodin.Theory.TeX where

import Rodin.Internal.Util
import Rodin.TeX
import Rodin.Theory
import Rodin.Formula.Tokenizer
import Rodin.Formula.TeX

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.NotationType'
instance ShowTeX NotationType where
  showTeX Prefix = ""
  showTeX Infix = "INFIX "

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.OperatorProp'
instance ShowTeX OperatorProp where
  showTeX op =
      (if as then " " ++ italic "associative" else "") ++
      (if co then " " ++ italic "commutative" else "") ++
      (if fo then " <expression>" else " <predicate>") ++
      " " ++ showTeX no
      where as = associative  op
            co = commutative  op
            fo = formulaType  op
            no = notationType op

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.OperatorArgument'
instance ShowTeX OperatorArgument where
  showTeX oa =
      id ++ ": " ++ printTeX ex
      where id = identifier oa
            ex = expression oa

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.OperatorWDCondition'
instance ShowTeX OperatorWDCondition where
  showTeX = printTeX . predicate

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.OperatorDirectDefinition'
instance ShowTeX OperatorDirectDefinition where
  showTeX odd =
      "\n" ++ ind 3 ++ "direct definition " ++ printTeXLines 4 fo
      where fo = formula odd

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.RecursiveDefinitionCase'
instance ShowTeX RecursiveDefinitionCase where
  showTeX rdc =
      "\n" ++ ind 4 ++ printTeX ex ++ " => " ++ printTeX fo
      where ex = caseExpression rdc
            fo = caseFormula    rdc

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.OperatorRecursiveDefinition'
instance ShowTeX OperatorRecursiveDefinition where
  showTeX ord =
      "\n" ++ ind 3 ++ "case " ++ ia ++ ":\n" ++ (texlist "" ca)
      where ia = inductiveArgument ord
            ca = cases             ord

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.ImportTheory'
instance ShowTeX ImportTheory where
  showTeX = theory

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.ImportTheoryProject'
instance ShowTeX ImportTheoryProject where
  showTeX itp =
      "\n" ++ ind 2 ++ pr ++ " THEORIES " ++ (texlist "," th)
      where pr = project  itp
            th = theories itp

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.TypeParameter'
instance ShowTeX TypeParameter where
  showTeX = typeIdentifier

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.ConstructorArgument'
instance ShowTeX ConstructorArgument where
  showTeX ca =
      id ++ ":" ++ printTeX ty
      where id = caId   ca
            ty = caType ca

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.DataTypeConstructor'
instance ShowTeX DataTypeConstructor where
  showTeX dtc =
      "\n" ++ ind 3 ++ id ++ "(" ++ (texlist "," ar) ++ ")"
      where id = dtcId dtc
            ar = args  dtc

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.TypeArgument'
instance ShowTeX TypeArgument where
  showTeX = printTeX . typeArg

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.DataTypeDefinition'
instance ShowTeX DataTypeDefinition where
  showTeX dtd =
      "\n" ++ ind 2 ++ id ++ (if not $ null ar then "(" ++ texlist "," ar ++ ")" else "") ++
      (if not $ null co then "\n" ++ ind 2 ++ "CONSTRUCTORS" ++ texlist "" co else "")
      where id = dtId          dtd
            ar = typeArguments dtd
            co = constructors  dtd

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.NewOperatorDefinition'
instance ShowTeX NewOperatorDefinition where
  showTeX nod =
      "\n" ++ ind 2 ++ (bold $ escape_ la) ++ showTeX pr ++ "(" ++ (texlist "," ar) ++ ")" ++
      (if not $ null wd then "\n" ++ ind 3 ++ "well-definedness " ++ texlist "," wd else "") ++
      (if not $ null di then texlist "" di else "") ++
      (if not $ null re then texlist "" re else "")
      where la = opLabel  nod
            pr = opProp   nod
            ar = opArgs   nod
            wd = opWD     nod
            di = opDirDef nod
            re = opRecDef nod

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.AxiomaticTypeDefinition'
instance ShowTeX AxiomaticTypeDefinition where
  showTeX = aTypeId


-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.AxiomaticOperatorDefinition'
instance ShowTeX AxiomaticOperatorDefinition where
  showTeX aod =
      "\n" ++ ind 3 ++ (bold $ escape_ la) ++ showTeX pr ++ "(" ++ (texlist "," ar) ++ ") : " ++ printTeX ty ++
      (if not $ null wd then "\n" ++ ind 4 ++ "well-definedness " ++ (texlist "," wd) else "")
      where la = aOpLabel aod
            pr = aOpProp  aod
            ty = aType    aod
            ar = aOpArgs  aod
            wd = aOpWD    aod


-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.AxiomaticDefinitionAxiom'
instance ShowTeX AxiomaticDefinitionAxiom where
  showTeX ada =
      "\n" ++ ind 3 ++ (italic $ escape_ la) ++ ": " ++ printTeXLines 4 pr
      where la = aDefLabel     ada
            pr = aDefPredicate ada

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.AxiomaticDefinitionsBlock'
instance ShowTeX AxiomaticDefinitionsBlock where
  showTeX adb =
      ind 1 ++ la ++ ":" ++
      (if not $ null ty then "\n" ++ ind 2 ++ "TYPES " ++ (texlist ", " ty) else "") ++
      (if not $ null de then "\n" ++ ind 2 ++ "OPERATORS" ++ (texlist "" de) else "") ++
      (if not $ null ax then "\n" ++ ind 2 ++ "AXIOMS" ++ (texlist "" ax) else "")
      where la = aDefBLabel adb
            ty = aDefBTypes adb
            de = aDefBDef   adb
            ax = aDefBAx    adb


-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.Theorem'
instance ShowTeX Theorem where
  showTeX th =
      "\n" ++ ind 2 ++ (italic $ escape_ na) ++ ": " ++ printTeXLines 3 pr
      where na = thName      th
            pr = thPredicate th

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.MetaVariable'
instance ShowTeX MetaVariable where
  showTeX mv =
      "\n" ++ ind 3 ++ id ++ ": " ++ printTeX ty
      where id = mvId   mv
            ty = mvType mv

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.InferenceGiven'
instance ShowTeX InferenceGiven where
  showTeX = printTeX . givenPredicate

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.InferenceInfer'
instance ShowTeX InferenceInfer where
  showTeX = printTeX . inferPredicate

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.InferenceRule'
instance ShowTeX InferenceRule where
  showTeX ir =
      "\n" ++ ind 3 ++ (italic $ escape_ la) ++ ": " ++ (texlist "," gi) ++ " $\\vdash$ " ++ (texlist "," ie)
      where la = infLabel ir
            ap = infApp   ir
            de = infDesc  ir
            gi = given    ir
            ie = infer    ir

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.RewriteRuleRHS'
instance ShowTeX RewriteRuleRHS where
  showTeX rh =
      "\n" ++ ind 4 ++ la ++ ": " ++ printTeX pr ++ " $\\Rightarrow$ " ++ printTeX fo
      where la = rhsLabel     rh
            pr = rhsPredicate rh
            fo = rhsFormula   rh

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.RewriteRule'
instance ShowTeX RewriteRule where
  showTeX rr =
      "\n" ++ ind 3 ++ (italic $ escape_ la) ++ ": " ++ printTeX ls ++ (texlist "" rs)
      where la = rewLabel rr
            ap = rewApp   rr
            co = complete rr
            de = rewDesc  rr
            ls = lhs      rr
            rs = rhs      rr

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.ProofRulesBlock'
instance ShowTeX ProofRulesBlock where
  showTeX prb =
      "\n" ++ ind 2 ++ la ++ ":" ++
      (if not $ null mv then "\n" ++ ind 2 ++ "Metavariables"   ++ (texlist "" mv) else "") ++
      (if not $ null rr then "\n" ++ ind 2 ++ "Rewrite Rules"   ++ (texlist "" rr) else "") ++
      (if not $ null ir then "\n" ++ ind 2 ++ "Inference Rules" ++ (texlist "" ir) else "")
      where la = prBLabel       prb
            mv = metaVariables  prb
            ir = inferenceRules prb
            rr = rewriteRules   prb

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Theory.Theory'
instance ShowTeX Theory where
  showTeX the =
      "THEORY " ++ na ++
      (if not $ null im then "\n" ++ ind 1 ++ "IMPORT THEORY PROJECTS" ++ (texlist "" im) else "") ++
      (if not $ null ty then "\n" ++ ind 1 ++ "TYPE PARAMETERS "       ++ (texlist "," ty) else "") ++
      (if not $ null da then "\n" ++ ind 1 ++ "DATA TYPES"             ++ (texlist "" da) else "") ++
      (if not $ null op then "\n" ++ ind 1 ++ "OPERATORS"              ++ (texlist "" op) else "") ++
      (if not $ null ax then "\n" ++ ind 1 ++ "AXIOMATIC DEFINITIONS"  ++ (texlist "" ax) else "") ++
      (if not $ null th then "\n" ++ ind 1 ++ "THEOREMS"               ++ (texlist "" th) else "") ++
      (if not $ null pr then "\n" ++ ind 1 ++ "PROOF RULES"            ++ (texlist "" pr) else "") ++
      "\nEND"
      where na = theoryName           the
            im = imports              the
            ty = typeParameters       the
            da = datatypeDefinitions  the
            op = operators            the
            ax = axiomaticDefinitions the
            th = theorems             the
            pr = proofRules           the





