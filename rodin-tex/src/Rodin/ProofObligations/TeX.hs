--------------------------------------------------------------------------
-- File: Rodin/ProofObligations/TeX.hs - Part of rodinapi-tex
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
Module      : Rodin.ProofObligations.TeX
Description : Module for printing a ProofObligations to LaTeX
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module mainly contains an instanciation of the 'Rodin.TeX.ShowTeX' for
the 'Rodin.ProofObligations' type.
-}
module Rodin.ProofObligations.TeX where

import Rodin.Internal.Util
import Rodin.TeX
import Rodin.ProofObligations
import qualified Rodin.ProofObligations.Ascii as A (guessName)
import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.Formula.TeX
import Data.List (intercalate)

-- | Guess the string corresponding to the origin of an antescedent.
-- Note that this function is already defined in 'Rodin.ProofObligations.Ascii', but
-- redefine it here to be able to bolden the name of the source.
guessName :: String -> String
guessName = bold . escape_ . A.guessName
--guessName "CTXHYP" = bold "Hypotheses from context:"
--guessName "ABSHYP" = bold "Hypotheses from abstract machine:"
--guessName "SEQHYP" = ""
--guessName x = bold (escape_ x) 

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.ProofObligations.POIdentifier'
instance ShowTeX POIdentifier where
  showTeX poi =
      math $ (escape_ na) ++ " " ++ (printTeX' ((TokOp OfType):(TokSpace SimpleSpace):ty))
      where na = poiName poi
            ty = poiType poi

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.ProofObligations.POPredicate'
instance ShowTeX POPredicate where
  showTeX pop =
      math
      $ (++) "\\hphantom{\\wedge}\\ "
      $ intercalate "$\n    $\\wedge\\ "
      $ map (printTeX')
      $ splitWhen (== TokOp And)
      $ removeAloneParentheses
      $ untype
      $ popPredicate pop

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.ProofObligations.POPredicateSet'
instance ShowTeX POPredicateSet where
  showTeX pops =
      (guessName na)
      ++ (if not $ null ids then "\n    let " ++ (intercalate "\n        " $ map showTeX ids) else "")
      ++ (if not $ null prs then "\n    hyp " ++ (intercalate "\n        " $ map showTeX prs) else "")
      ++ "\n"
      where na  = popsName        pops
            ids = popsIdentifiers pops
            prs = popsPredicates  pops

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.ProofObligations.POSequent'
instance ShowTeX POSequent where
  showTeX pos =
      bold (escape_ na) ++ ":" ++ (if not $ null de then " -- " ++ de else "")
      ++ (intercalate "" $ map showTeX ps)
      ++ "$\\vdash$\n"
      ++ "    " ++ (intercalate "\n    " $ map showTeX prs)
      ++ "\n"
      where na  = posName          pos
            de  = posDesc          pos
            ps  = posPredicateSets pos
            prs = posPredicates    pos

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.ProofObligations.POFile'
instance ShowTeX POFile where
  showTeX pof =
      (intercalate "\n" $ map showTeX prs)
      ++ ("---\n")
      ++ (intercalate "\n" $ map showTeX seqs)
      where prs  = pofPredicateSets pof
            seqs = pofSequents      pof



