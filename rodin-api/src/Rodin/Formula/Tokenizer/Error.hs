------------------------------------------------------------------------
-- File: Rodin/Formula/Tokenizer/Error.hs - Part of rodinapi
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
Module      : Rodin.Formula.Tokenizer.Error
Description : Module for mangaging possible errors when tokenizing formulas
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module provides types for managing errors that could occure when parsing a formula.
This module is highly dependant on 'Rodin.Internal.Wrap'
-}
module Rodin.Formula.Tokenizer.Error (
    FormulaParseError,FormulaParseWarning,
    fpe_position,fpe_fragment,fpe_message,
    FPWrap,failwith_
    ) where

import Rodin.Internal.Wrap
import Rodin.Formula.Tokenizer.FTk
    
-- | An error arrising when parsing a formula
data FormulaParseError = FormulaParseError {
    fragment :: FTk,        -- ^ Erroneous token
    message :: String       -- ^ Error message
}

-- | Get the position of the error
fpe_position :: FormulaParseError -> Int
fpe_position = ftkposition . fragment

-- | Get the erroneous input fragment
fpe_fragment :: FormulaParseError -> String
fpe_fragment = ftkcontent . fragment

-- | Get the error message
fpe_message :: FormulaParseError -> String
fpe_message = message

-- | Convenient 'Show' instance for the 'FormulaParseError'
instance Show FormulaParseError where
  show pe =
      "Error:" ++ show (ftkposition $ fragment pe) ++ ": at '" ++ (ftkcontent $ fragment pe) ++ "', " ++ (message pe)

-- | Empty 'FormulaParseWarning' type (no warning available when parsing a formula) for building the 'Rodin.Internal.Wrap.Wrap' type
data FormulaParseWarning

-- | 'Rodin.Internal.Wrap.Wrap' type synonym
type FPWrap = Wrap FormulaParseError FormulaParseWarning

-- | 'Rodin.Internal.Wrap.failwith' override for convenient use
failwith_ :: FTk -> String -> FPWrap a
failwith_ = failwith' FormulaParseError

