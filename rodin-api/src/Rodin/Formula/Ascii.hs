------------------------------------------------------------------------
-- File: Rodin/Formula/Ascii.hs - Part of rodinapi
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
Module      : Rodin.Formula.Ascii
Description : Ascii converter for Event-B formulas
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.com

Implementation of the ASCII typeclass defined in 'Rodin.Ascii' for formulas
-}
{-# LANGUAGE FlexibleInstances,TypeSynonymInstances #-}
module Rodin.Formula.Ascii where

import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.Internal.Util
import Rodin.Ascii
import Data.List (intercalate)

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Formula.Operator'
instance ShowAscii Operator where
  showAscii Top                           = "TRUE"
  showAscii Bottom                        = "FALSE"
  showAscii And                           = "&"
  showAscii Or                            = "or"
  showAscii Implies                       = "=>"
  showAscii Equivalent                    = "<=>"
  showAscii Not                           = "not"
  showAscii ForAll                        = "!"
  showAscii Exists                        = "#"
  showAscii Equal                         = "="
  showAscii NotEqual                      = "/="
  showAscii In                            = ":"
  showAscii NotIn                         = "/:"
  showAscii EmptySet                      = "{}"
  showAscii SubSetEq                      = "<:"
  showAscii NoSubSetEq                    = "/<:"
  showAscii SubSetStrict                  = "<<:"
  showAscii NotSubSetStrict               = "/<<:"
  showAscii Union                         = "\\/"
  showAscii Intersection                  = "/\\"
  showAscii Difference                    = "\\"
  showAscii Powerset                      = "POW"
  showAscii Powerset1                     = "POW1"
  showAscii GenUnion                      = "UNION"
  showAscii GenIntersection               = "INTER"
  showAscii Maplet                        = "|->"
  showAscii CartesianProduct              = "*"
  showAscii Relation                      = "<->"
  showAscii TotalRelation                 = "<<->" 
  showAscii SurjectiveRelation            = "<->>"
  showAscii TotalSurjectiveRelation       = "<<->>"
  showAscii DomainRestriction             = "<|"
  showAscii DomainSubtraction             = "<<|"
  showAscii RangeRestriction              = "|>"
  showAscii RangeSubtraction              = "|>>"
  showAscii RelationalForwardComposition  = ";"
  showAscii RelationalBackwardComposition = "circ"
  showAscii RelationalOverride            = "<+"
  showAscii ParallelProduct               = "||"
  showAscii DirectProduct                 = "><"
  showAscii Inverse                       = "~"
  showAscii PartialFunction               = "+->"
  showAscii TotalFunction                 = "-->"
  showAscii PartialInjection              = ">+>"
  showAscii TotalInjection                = ">->"
  showAscii PartialSurjection             = "+->>"
  showAscii TotalSurjection               = "-->>"
  showAscii Bijection                     = ">->>"
  showAscii Lambda                        = "%"
  showAscii Range                         = ".."
  showAscii Plus                          = "+"
  showAscii Minus                         = "-"
  showAscii Multiply                      = "*"
  showAscii Division                      = "/"
  showAscii Exponent                      = "^"
  showAscii Lower                         = "<"
  showAscii LowerEq                       = "<="
  showAscii Greater                       = ">"
  showAscii GreaterEq                     = ">="
  showAscii Assignment                    = ":="
  showAscii BeforeAfterPredicate          = ":|"
  showAscii SetMemberAssignment           = "::"
  showAscii OfType                        = ":"

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Formula.SpecialIdent'
instance ShowAscii SpecialIdent where
  showAscii Integers = "INTEGER"
  showAscii Naturals = "NATURAL"
  showAscii Naturals1 = "NATURAL1"

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Formula.Space'
instance ShowAscii Space where
  showAscii SimpleSpace = " "
  showAscii Newline = "\n"
  showAscii Tab = ind 1         -- Note that "tabulations" are represented by spaces to avoid problems with editors

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Formula.SimpleToken'
instance ShowAscii SimpleToken where
  showAscii TokOpenPar = "("
  showAscii TokClosePar = ")"
  showAscii TokOpenBra = "["
  showAscii TokCloseBra = "]"
  showAscii TokOpenCBra = "{"
  showAscii TokCloseCBra = "}"
  showAscii TokComa = ","
  showAscii TokDot = "."
  showAscii TokMid = "|"

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Formula.Token'
instance ShowAscii Token where
  showAscii TokUndef = ""
  showAscii (TokOp op) = showAscii op
  showAscii (TokSpecialIdent sp) = showAscii sp
  showAscii (TokOpIdent op) = op
  showAscii (TokIdent id) = id
  showAscii (TokSpace s) = showAscii s
  showAscii (TokToken t) = showAscii t

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Formula.Formula'
instance ShowAscii Formula where
  showAscii = printAscii


-- | Print a list of token (hence a formula) as ASCII with no further treatment
printAscii :: [Token] -> String
printAscii = foldl (\acc -> \x -> acc ++ (showAscii x)) ""

-- | Print a list of token (hence a formula) as ASCII with no further treatment
printAsciiC :: Maybe String -> [Token] -> String
printAsciiC c tks =
    let as = printAscii tks in
        case c of
          Nothing -> as
          Just c -> as ++ " -- " ++ c

-- | Print a list of token (hence a formula) as ASCII, providing a list of the resulting lines (without newline character)
printAsciiLines' :: [Token] -> [String]
printAsciiLines' = printLines' printAscii

-- | Print a list of token (hence a formula) as ASCII, indenting each line with the given number of spaces
printAsciiLines :: Int -> [Token] -> String
printAsciiLines = printLines printAscii

-- | Print a list of token (hence a formula) as ASCII, indenting each line with the given number of spaces, and put a comment (if applicable) at the end of the first line
printAsciiLinesC :: Maybe String -> Int -> [Token] -> String
printAsciiLinesC Nothing ind tks = printAsciiLines ind tks
printAsciiLinesC (Just c) ind tks =
    case printLines' printAscii tks of
      [] -> ""
      (x:xs) -> let x' = x ++ " -- " ++ c in (x':xs)

-- | Print an ascii representation of the list of tokens (plus possible comments), on multiple
-- lines if needed.
printAsciiAutoC :: Maybe String -> Int -> [Token] -> String
printAsciiAutoC = printAutoC printAsciiC printAsciiLinesC



