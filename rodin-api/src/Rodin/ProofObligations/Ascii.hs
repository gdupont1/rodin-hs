------------------------------------------------------------------------
-- File: Rodin/ProofObligations/Ascii.hs - Part of rodinapi
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
Module      : Rodin.ProofObligations.Ascii
Description : Module for converting Rodin POs in ASCII
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module converts (/ "pretty prints") a Rodin PO file in ASCII
-}
module Rodin.ProofObligations.Ascii where

import Rodin.Internal.Util
import Rodin.Ascii
import Rodin.ProofObligations
import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.Formula.Ascii
import Data.List (intercalate)

-- | Guess the string corresponding to the origin of an antecedent
guessName :: String -> String
guessName "CTXHYP" = "Hypotheses from context:"
guessName "ABSHYP" = "Hypotheses from abstract machine:"
guessName "SEQHYP" = ""
guessName x = x

-- | 'Rodin.ShowAscii' instance for 'Rodin.ProofObligations.POIdentifier'
instance ShowAscii POIdentifier where
  showAscii id =
      na ++ " " ++ (printAscii ((TokOp OfType):(TokSpace SimpleSpace):ty))
      where na = poiName id
            ty = poiType id

-- | 'Rodin.ShowAscii' instance for 'Rodin.ProofObligations.POPredicate'
instance ShowAscii POPredicate where
  showAscii = printAscii . popPredicate

-- | 'Rodin.ShowAscii' instance for 'Rodin.ProofObligations.POPredicateSet'
instance ShowAscii POPredicateSet where
  showAscii pops =
      (guessName na)
      ++ (if not $ null ids then "\n    let " ++ (intercalate "\n        " $ map showAscii ids) else "")
      ++ (if not $ null prs then "\n    hyp " ++ (intercalate "\n        " $ map showAscii prs) ++ "\n" else "")
      where na  = popsName        pops
            ids = popsIdentifiers pops
            prs = popsPredicates  pops

-- | 'Rodin.ShowAscii' instance for 'Rodin.ProofObligations.POSequent'
instance ShowAscii POSequent where
  showAscii pos =
      na ++ ":" ++ (if not $ null de then " -- " ++ de else "")
      ++ (intercalate "" $ map showAscii ps)
      ++ "|-\n"
      ++ "    " ++ (intercalate "\n    " $ map showAscii prs)
      ++ "\n"
      where na  = posName          pos
            de  = posDesc          pos
            ps  = posPredicateSets pos
            prs = posPredicates    pos

-- | 'Rodin.ShowAscii' instance for 'Rodin.ProofObligations.POFile'
instance ShowAscii POFile where
  showAscii pof =
      (intercalate "\n" $ map showAscii prs)
      ++ ("---\n")
      ++ (intercalate "\n" $ map showAscii seqs)
      where prs  = pofPredicateSets pof
            seqs = pofSequents      pof



