------------------------------------------------------------------------
-- File: Rodin/Formula.hs - Part of rodinapi
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
Module      : Rodin.Formula
Description : module for representing a general Event-B formula
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This top level module mainly contains the datatypes representing a formula.
Submodules contains other functionalities like tokenization or printing.
-}
module Rodin.Formula where

-- | An Event-B operator
data Operator =
      Top
    | Bottom
    | And
    | Or
    | Implies
    | Equivalent
    | Not
    | ForAll
    | Exists
    | Equal
    | NotEqual
    | In
    | NotIn
    | EmptySet
    | SubSetEq
    | NoSubSetEq
    | SubSetStrict
    | NotSubSetStrict
    | Union
    | Intersection
    | Difference
    | Powerset
    | Powerset1
    | GenUnion
    | GenIntersection
    | Maplet
    | CartesianProduct
    | Relation
    | TotalRelation
    | SurjectiveRelation
    | TotalSurjectiveRelation
    | DomainRestriction
    | DomainSubtraction
    | RangeRestriction
    | RangeSubtraction
    | RelationalForwardComposition
    | RelationalBackwardComposition
    | RelationalOverride
    | ParallelProduct
    | DirectProduct
    | Inverse
    | PartialFunction
    | TotalFunction
    | PartialInjection
    | TotalInjection
    | PartialSurjection
    | TotalSurjection
    | Bijection
    | Lambda
    | Range
    | Plus
    | Minus
    | Multiply
    | Division
    | Exponent
    | Lower
    | LowerEq
    | Greater
    | GreaterEq
    | Assignment
    | BeforeAfterPredicate
    | SetMemberAssignment
    | OfType
    deriving (Eq,Ord,Enum,Show)

-- | An reserved identifier that should be represented by a special symbol
data SpecialIdent =
      Integers
    | Naturals
    | Naturals1
    deriving (Eq,Ord,Enum,Show)

-- | Some kind of white space
data Space =
      SimpleSpace
    | Newline
    | Tab
    deriving (Eq,Ord,Enum,Show)

-- | Other types of tokens, mainly parentheses/brackets, comas, dot and mid (used as delimiters)
data SimpleToken =
      TokOpenPar
    | TokClosePar
    | TokOpenBra
    | TokCloseBra
    | TokOpenCBra
    | TokCloseCBra
    | TokComa
    | TokDot
    | TokMid
    deriving (Eq,Ord,Enum,Show)

-- | Main type : a token in the formula
data Token =
      TokUndef                         -- ^ Undefined token
    | TokOp Operator                   -- ^ Operator
    | TokSpecialIdent SpecialIdent     -- ^ Special identifier
    | TokOpIdent String                -- ^ Identifier corresponding to an operator
    | TokIdent String                  -- ^ Any other identifier
    | TokSpace Space                   -- ^ A white space
    | TokToken SimpleToken             -- ^ Some other "top-level" token
    deriving (Show,Eq)

-- | A formula is a list of tokens
type Formula = [Token]



