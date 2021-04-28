{-|
Module      : Rodin.Formula.Tokenizer.Error
Description : Module for mangaging possible errors when tokenizing formulas
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module provides types for managing errors that could occure when parsing a formula.
-}
module Rodin.Formula.Tokenizer.Error (
    ErrorType(..),ParseError(..),ParseResult(..),
    fpe_position,fpe_fragment,fpe_type
    ) where

import Rodin.Formula.Tokenizer.FTk

-- | Type of errors that may occur during parsing
data ErrorType =    
      UnclosedQuote                 -- ^ Quote has not been closed
    | UnexpectedOperator String     -- ^ Unexpected operator at this position
    | UnknownOperator               -- ^ Impossible to parse tokens

-- | Turn an error type into a string
errstring :: ErrorType -> String
errstring UnclosedQuote = "unclosed quote"
errstring (UnexpectedOperator op) = "unexpected operator '" ++ op ++ "'"
errstring (UnknownOperator) = "unknown operator(s)"

-- | An error arrising when parsing a formula
data ParseError = ParseError {
    fragment :: FTk,        -- ^ Erroneous token
    errorType :: ErrorType  -- ^ Error type
}

-- | Get the position of the error
fpe_position :: ParseError -> Int
fpe_position = ftkposition . fragment

-- | Get the erroneous input fragment
fpe_fragment :: ParseError -> String
fpe_fragment = ftkcontent . fragment

-- | Get the error type
fpe_type :: ParseError -> ErrorType
fpe_type = errorType

-- | Convenient 'Show' instance for the 'FormulaParseError'
instance Show ParseError where
  show pe =
      "Error:" ++ show (ftkposition $ fragment pe) ++ ": at '" ++ (ftkcontent $ fragment pe) ++ "', " ++ (errstring $ errorType pe)

-- | Result of parsing a formula.
-- Type synonym for `Either` with `ParseError` on the left
type ParseResult a = Either ParseError a


