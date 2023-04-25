------------------------------------------------------------------------
-- File: Rodin/Proofs/Ascii.hs - Part of rodinapi
------------------------------------------------------------------------
-- Copyright (C) 2023  G. Dupont
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
Module      : Rodin.Proofs.Ascii
Description : Module for converting Rodin proofs in ASCII
Copyright   : Copyright (C) 2023 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module converts (/ "pretty prints") a Rodin proof file in ASCII
-}
module Rodin.Proofs.Ascii where

import Rodin.Internal.Util
import Rodin.Ascii
import Rodin.Proofs
import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.Formula.Ascii
import Data.List (intercalate)

instance ShowAscii PrRule where
  showAscii rule =
      "  Rule " ++ di ++ " (" ++ na ++ ", confidence " ++ co ++ ")" ++ "\n"
      ++ "   hyp: " ++ (intercalate ", " hy) ++ "\n"
      ++ "   goal: " ++ go ++ "\n"
      ++ "\n" ++ (intercalate "\n" $ map showAscii an) ++ "\n"
      where na = ruleName rule
            co = show $ ruleConfidence rule
            di = ruleDisplay rule
            hy = ruleHypotheses rule
            go = 
                case ruleGoal rule of
                  Nothing -> "-"
                  Just g -> g
            an = ruleAnte rule

instance ShowAscii PrAnte where
  showAscii ante =
      "  Branch " ++ na ++ "\n"
      ++ "   hyp: " ++ (intercalate ", " hy) ++ "\n"
      ++ "   goal: " ++ go ++ "\n"
      ++ "   ids: " ++ (intercalate ", " $ map showAscii is) ++ "\n"
      ++ "   actions: " ++ (intercalate "\n            " $ map showAscii ha)
      ++ showAscii ru ++ "\n"
      ++ "  end " ++ na ++ "\n"
      where na = anteName ante
            hy = anteHypotheses ante
            go = 
                case anteGoal ante of
                  Nothing -> "-"
                  Just g -> g
            ha = anteHypActions ante
            is = anteIdents ante
            ru = anteRule ante

instance ShowAscii PrHypAction where
  showAscii ha =
      na ++ "[" ++ (intercalate ", " hi) ++ "] [" ++ (intercalate ", " hy) ++ "] [" ++ (intercalate ", " ih ) ++ "]"
      where na = haName ha
            hi = haHidden ha
            hy = haHypotheses ha
            ih = haInfHypotheses ha

instance ShowAscii PrIdent where
  showAscii pi =
      na ++ " : " ++ showAscii ty
      where na = pridName pi
            ty = pridType pi

instance ShowAscii PrPredicate where
  showAscii pr =
      na ++ "(" ++ (intercalate "," $ map showAscii is) ++ ") == " ++ showAscii pp
      where na = prName pr
            pp = prPred pr
            is = prIdents pr

instance ShowAscii PrReas where
  showAscii reas =
      na ++ "<" ++ ri ++ ">"
      where na = reasName reas
            ri = reasRID reas

instance ShowAscii Proof where
  showAscii pr =
      "Proof for " ++ na ++ " (confidence: " ++ co ++ ", manual: " ++ ma ++ ")\n"
      ++ "  Goal: " ++ go ++ "\n"
      ++ "  Fresh variables: " ++ (intercalate ", " fr) ++ "\n"
      ++ "  Hypotheses: " ++ (intercalate ", " hy) ++ "\n"
      ++ "  Predicate:\n" ++ (intercalate "\n  - " $ map showAscii pp) ++ "\n"
      ++ "  Identifiers:\n" ++ (intercalate "\n  - " $ map showAscii pi) ++ "\n"
      ++ "  Reas:\n" ++ (intercalate "\n  - " $ map showAscii pu) ++ "\n"
      ++ showAscii en
      where na = proofName pr
            co = show $ proofConfidence pr
            fr = proofFresh pr
            go = proofGoal pr
            hy = proofHypotheses pr
            ma = if proofManual pr then "yes" else "no"
            en = proofEntry pr
            pp = proofPredicates pr
            pi = proofIdentifiers pr
            pu = proofReas pr

instance ShowAscii ProofFile where
  showAscii pf =
      "Proofs for " ++ na ++ "\n====================\n\n"
      ++ (intercalate "\n" $ map showAscii ps) ++ "\n"
      where na = pfName pf
            ps = pfProofs pf


