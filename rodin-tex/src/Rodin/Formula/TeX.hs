--------------------------------------------------------------------------
-- File: Rodin/Formula/TeX.hs - Part of rodinapi-tex
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
Module      : Rodin.Formula.TeX
Description : Module for printing a Formula to LaTeX
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module mainly contains an instanciation of the 'Rodin.TeX.ShowTeX' for
the 'Rodin.Formula' type. It also contains a few tricks to get the formula
well printed.
-}
module Rodin.Formula.TeX where

import Rodin.Internal.Util
import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.TeX
import Data.List (intercalate)
import Data.Char (isAlpha,isAlphaNum)

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Formula.Operator' type.
-- Important note: the generated LaTeX is *not* standalone. It requires
-- additionnal macros, in particular for:
--   * @\leftleftrightarrow@ == @\leftarrow\mkern-14mu\leftrightarrow@
--   * @\leftrightrightarrow@ == @\leftrightarrow\mkern-14mu\rightarrow@
--   * @\leftleftrightrightarrow@ == @\leftrightarrow\mkern-14mu\leftrightarrow@
--   * @\lhdminus@ == @\lhd\mkern-14mu-@
--   * @\rhdminus@ == @\rhd\mkern-14mu-@
--   * @\lhdplus@ == @\lhd\mkern-9mu-@
--   * @\partialrightarrow@ == @\mkern6mu\mapstochar\mkern-6mu\rightarrow@
--   * @\partialrightarrowtail@ == @\mkern9mu\mapstochar\mkern-9mu\rightarrowtail@
--   * @\partialtwoheadrightarrow@ == @\mkern6mu\mapstochar\mkern-6mu\twoheadrightarrow@
--   * @\twoheadrightarrowtail@ == @\rightarrowtail\mkern-18mu\twoheadrightarrow@
instance ShowTeX Operator where
  showTeX Top = "\\top"
  showTeX Bottom = "\\bot"
  showTeX And = "\\wedge"
  showTeX Or = "\\vee"
  showTeX Implies = "\\Rightarrow"
  showTeX Equivalent = "\\Leftrightarrow"
  showTeX Not = "\\neg"
  showTeX ForAll = "\\forall"
  showTeX Exists = "\\exists"
  showTeX Equal = "="
  showTeX NotEqual = "\\neq"
  showTeX In = "\\in"
  showTeX NotIn = "\\notin"
  showTeX EmptySet = "\\emptyset"
  showTeX SubSetEq = "\\subseteq"
  showTeX NoSubSetEq = "\\not\\subseteq"
  showTeX SubSetStrict = "\\subset"
  showTeX NotSubSetStrict = "\\not\\subset"
  showTeX Union = "\\cup"
  showTeX Intersection = "\\cap"
  showTeX Difference = "\\setminus"
  showTeX Powerset = "\\mathbb{P}"
  showTeX Powerset1 = "\\mathbb{P}1"
  showTeX GenUnion = "\\bigcup"
  showTeX GenIntersection = "\\bigcap"
  showTeX Maplet = "\\mapsto"
  showTeX CartesianProduct = "\\times"
  showTeX Relation = "\\leftrightarrow"
  showTeX TotalRelation = "\\leftleftrightarrow" -- "\\leftarrow\\mkern-14mu\\leftrightarrow"
  showTeX SurjectiveRelation = "\\leftrightrightarrow" --"\\leftrightarrow\\mkern-14mu\\rightarrow"
  showTeX TotalSurjectiveRelation = "\\leftleftrightrightarrow" -- "\\leftrightarrow\\mkern-14mu\\leftrightarrow"
  showTeX DomainRestriction = "\\lhd"
  showTeX DomainSubtraction = "\\lhdminus" -- "\\lhd\\mkern-14mu-"
  showTeX RangeRestriction = "\\rhd"
  showTeX RangeSubtraction = "\\rhdminus" -- "\\rhd\\mkern-14mu-"
  showTeX RelationalForwardComposition = ";"
  showTeX RelationalBackwardComposition = "\\circ"
  showTeX RelationalOverride = "\\lhdplus" -- "\\lhd\\mkern-9mu-"
  showTeX ParallelProduct = "\\parallel"
  showTeX DirectProduct = "\\otimes"
  showTeX Inverse = "^{-1}"
  showTeX PartialFunction = "\\partialrightarrow" -- "\\mkern6mu\\mapstochar\\mkern-6mu\\rightarrow"
  showTeX TotalFunction = "\\rightarrow"
  showTeX PartialInjection = "\\partialrightarrowtail" -- "\\mkern9mu\\mapstochar\\mkern-9mu\\rightarrowtail"
  showTeX TotalInjection = "\\rightarrowtail"
  showTeX PartialSurjection = "\\partialtwoheadrightarrow" -- "\\mkern6mu\\mapstochar\\mkern-6mu\\twoheadrightarrow"
  showTeX TotalSurjection = "\\twoheadrightarrow"
  showTeX Bijection = "\\twoheadrightarrowtail" -- "\\rightarrowtail\\mkern-18mu\\twoheadrightarrow"
  showTeX Lambda = "\\lambda"
  showTeX Range = ".."
  showTeX Plus = "+"
  showTeX Minus = "-"
  showTeX Multiply = "."
  showTeX Division = "\\div"
  showTeX Exponent = "\\^"
  showTeX Lower = "<"
  showTeX LowerEq = "\\leq"
  showTeX Greater = ">"
  showTeX GreaterEq = "\\geq"
  showTeX Assignment = ":="
  showTeX BeforeAfterPredicate = ":\\mid"
  showTeX SetMemberAssignment = ":\\in"
  showTeX OfType = ":"

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Formula.SpecialIdent' type
instance ShowTeX SpecialIdent where
  showTeX Integers = "\\mathbb{Z}"
  showTeX Naturals = "\\mathbb{N}"
  showTeX Naturals1 = "\\mathbb{N}1"

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Formula.Space' type
instance ShowTeX Space where
  showTeX SimpleSpace = " "
  showTeX Newline = "\n"
  showTeX Tab = ind 1

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Formula.SimpleToken' type
instance ShowTeX SimpleToken where
  showTeX TokOpenPar = "("
  showTeX TokClosePar = ")"
  showTeX TokOpenBra = "["
  showTeX TokCloseBra = "]"
  showTeX TokOpenCBra = "{"
  showTeX TokCloseCBra = "}"
  showTeX TokComa = ","
  showTeX TokDot = "\\cdot"
  showTeX TokMid = "\\mid"

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Formula.Token' type
instance ShowTeX Token where
  showTeX TokUndef = ""
  showTeX (TokOp op) = showTeX op
  showTeX (TokSpecialIdent sp) = showTeX sp
  showTeX (TokOpIdent op) = "\\mathrm{" ++ op ++ "}"
  showTeX (TokIdent id) = id
  showTeX (TokSpace s) = showTeX s
  showTeX (TokToken t) = showTeX t

-- | Function that prints a list of tokens as a LaTeX string. The resulting
-- string is correct LaTeX (modulo additionnal macros) and is in particular
-- correctly escaped/math'd.
printTeX' :: [Token] -> String
printTeX' tks =
    print1 "" tks
    --    print1 previous current
    where print1 _  []     = ""
          print1 st (x:xs) =
              let xx = xShowTeX x
                in (space st xx) ++ xx ++ (print1 xx xs)
          space st xx =
              case st of
                '\\':_ -> if isAlphaNum $ head xx then " " else ""
                _      -> ""
          xShowTeX (TokIdent id) =
              case id of
                [] -> ""
                ('#':xs) -> xs
                (x:xs) | null xs || (not $ isAlpha x) -> id
                --_ | '"' `elem` id ->
                _ | otherwise -> "\\mathit{" ++ escape_ id ++ "}"
          xShowTeX (TokToken TokOpenCBra) = "\\{"
          xShowTeX (TokToken TokCloseCBra) = "\\}"
          xShowTeX x = showTeX x

-- | Function that prints a list of tokens as a LaTeX string.
printTeX :: [Token] -> String
printTeX tks =
    withMath False tks
    where withMath m [] = if m then "$" else ""
          withMath m (x:xs) =
              let ism = isMath x in
                  let next = withMath ism xs in
                      (if (m && ism) || ((not m) && (not ism)) then "" else "$") ++ showTeX x ++ next
          -- withMath (math mode on) (tokens)

-- | Prints a list of tokens as a LaTeX string. The resulting string respects the
-- provided newline tokens, and every line is indented by the given number of spaces.
-- Identation is added before any dollar sign.
printTeXLines'' :: Int -> [Token] -> String
printTeXLines'' = printLines (mathspace . printTeX')

-- | Prints a list of tokens as a list of LaTeX strings, each of which is a line
-- (with no newline character).
printTeXLines' :: [Token] -> [String]
printTeXLines' = printLines' printTeX

-- | Prints a list of tokens as a LaTeX string. The resulting string respects the
-- provided newline tokens, and every line is indented by the given number of spaces.
printTeXLines :: Int -> [Token] -> String
printTeXLines = printLines printTeX


