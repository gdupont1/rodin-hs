------------------------------------------------------------------------
-- File: Rodin/Formula/Tokenizer/FTk.hs - Part of rodinapi
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
Module      : Rodin.Formula.Tokenizer.FTk
Description : Internal types used by the tokenizer
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module mainly defines types used by the tokenizer as internal representation
(i.e. mainly 'Nature' and 'FTk') as well as simple functions on them.
-}
module Rodin.Formula.Tokenizer.FTk where


import Rodin.Formula
import qualified Data.Char as C (isAlpha,isNumber,isSpace)

-- | Detect if the token is a space (simple space, tab or newline)
isSpaceTk :: Token -> Bool
isSpaceTk (TokSpace _) = True
isSpaceTk _ = False

-- | Token annotation: store the "suspected" type of token being parsed
data Nature =
      Void                  -- ^ No type
    | Number                -- ^ Number
    | Ident                 -- ^ Identifier (sequence of alphanumerical characters)
    | Operator              -- ^ Operator
    | Quote                 -- ^ Quotation mark (')
    | Space                 -- ^ A space
    deriving (Enum,Eq)

-- | 'Show' instance for 'Nature' for debugging purposes
instance Show Nature where
  show Void = "V"
  show Number = "N"
  show Ident = "I"
  show Operator = "O"
  show Space = "S"
  show Quote = "Q"

-- | Detect if given character could be part of an identifier.
-- *Fix:* we have to exclude `λ` (U+03BB, lambda) as it is an alpha
-- character!
isIdentChar :: Char -> Bool
isIdentChar 'λ' = False
isIdentChar c = C.isAlpha c || c == '_' || c == '\'' || c == '$'

-- | Determine the nature of given character
nature :: Char -> Nature
nature c
    | isIdentChar c = Ident
    | C.isNumber c = Number
    | C.isSpace c = Space
    | c == '"' = Quote
    | otherwise = Operator

-- | Internal representation of a "formula token", precursor of propers 'Formula.Token'
data FTk = FTk {
    ftkcontent  :: String,          -- ^ String containing the full token
    ftknature   :: Nature,          -- ^ Suspected or definitive nature
    ftkposition :: Int              -- ^ Position (i.e. character number) in the parsed string
}


-- | "Lifted" version of the ':' operator for pre-pending a character to the stirng contained in
-- a FTk
(+:) :: Char -> FTk -> FTk
c +: ftk = ftk { ftkcontent = c:(ftkcontent ftk) }

infixr 5 +:

-- | Reverse the content of an FTk while retaining its other fields
reversef :: FTk -> FTk
reversef ftk = ftk { ftkcontent = reverse (ftkcontent ftk) }

-- | 'Show' instance of FTk for debugging purposes
instance Show FTk where
  show (FTk ct na po) = show ct ++ "[[" ++ show na ++ "]]<<" ++ show po ++ ">>"




