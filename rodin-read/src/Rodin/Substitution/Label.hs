{-|
Module      : Rodin.Substitution.Label
Description : specification of the substitution system for formulas
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

This module defines a @Substitution.Label.Matcher@ instance for
@Rodin.Formula.Token@.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Rodin.Substitution.Label (
        Label,
        parseLabel
    ) where

import Rodin.Ascii
import Rodin.TeX
import Rodin.Formula
import Rodin.Formula.UTF8
import Rodin.Formula.Ascii
import Rodin.Formula.TeX
import Data.Char (isSpace)
import Substitution.Label (Matcher, (//>))

-- | Labels of the patterns for the substitution rules
data Label =
      AnyToken              -- ^ Any token (.)
    | AnyOperator           -- ^ Any operator
    | AnySpecialIdent       -- ^ Any special identifier
    | AnyOpIdent            -- ^ Any operator identifier
    | AnyIdent              -- ^ Any identifier
    | AnySpace              -- ^ Any space
    | AnySimpleToken        -- ^ Any "simple" token (delimiters)
    | Exactly Token         -- ^ Exactly the given token
    | PrintsLike String     -- ^ A token that "prints like" the specified string
    deriving Eq

-- | Returns true if the label is general (for calculating order)
isAny :: Label -> Bool
isAny (Exactly _)    = False
isAny (PrintsLike _) = False
isAny _              = True

instance Show Label where
  show AnyToken        = "<token>"
  show AnyOperator     = "<operator>"
  show AnySpecialIdent = "<special>"
  show AnyOpIdent      = "<op_id>"
  show AnyIdent        = "<id>"
  show AnySpace        = "<space>"
  show (Exactly t)     = show t
  show (PrintsLike s)  = "'" ++ s ++ "'"

instance Ord Label where
  -- | Compare two labels. Basically, labels that are general are lower than
  -- specific labels.
  compare a b
      | a == b = EQ
      | isAny a && isAny b = EQ
      | isAny a = LT
      | isAny b = GT
      | otherwise = EQ

-- | Matcher instance.
instance Matcher Label Token where
  AnyToken        //> _                   = True
  AnyOperator     //> (TokOp           _) = True
  AnySpecialIdent //> (TokSpecialIdent _) = True
  AnyOpIdent      //> (TokOpIdent      _) = True
  AnyIdent        //> (TokIdent        _) = True
  AnySpace        //> (TokSpace        _) = True
  AnySimpleToken  //> (TokToken        _) = True
  (Exactly tk)    //> tk'                 = (tk == tk')
  (PrintsLike s)  //> tk'                 = (showAscii tk' == s) || (showUTF8 tk' == s) || (showTeX tk' == s)
  _ //> _ = False

-- | Parse a string into a list of labels, or return Nothing if there is an
-- error.
parseLabel :: String -> Maybe [Label]
parseLabel [] = Just []
parseLabel ('\\':x:xs)
    | x == 'o' = (AnyOperator:) <$> parseLabel xs
    | x == 'I' = (AnySpecialIdent:) <$> parseLabel xs
    | x == 'i' = (AnyIdent:) <$> parseLabel xs
    | x == 's' = (AnySpace:) <$> parseLabel xs
    | x == 'k' = (AnySimpleToken:) <$> parseLabel xs
    | x `elem` ".*|()!?<>\\\"" = ((PrintsLike [x]):) <$> parseLabel xs
    | otherwise = Nothing
parseLabel ('.':xs) = (AnyToken:) <$> parseLabel xs
parseLabel l =
    let (one, rem) = span (not . isSpace) l in
        ((PrintsLike one):) <$> (parseLabel $ dropWhile isSpace rem)




