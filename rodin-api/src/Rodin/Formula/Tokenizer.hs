------------------------------------------------------------------------
-- File: Rodin/Formula/Tokenizer.hs - Part of rodinapi
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
Module      : Rodin.Formula.Tokenizer
Description : Module for tokenizing an UTF-8 sentence to a formula
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module provides functions for extracting a list of tokens from a sentence
in UTF-8, typically coming from a Rodin file.
-}
module Rodin.Formula.Tokenizer (
    isIdentChar,
    tokenize,
    tkn,
    replaceEntities
    ) where

import Rodin.Formula
import Rodin.Formula.Tokenizer.FTk
import Rodin.Formula.Tokenizer.Error
import Rodin.Formula.UTF8
import Rodin.Formula.Ascii
import Data.List (groupBy)
import Data.Either.Combinators (fromRight, isLeft)
import Data.Maybe (fromJust)
import Data.Char (chr)
import Data.Function (on)

{-| 
The transformation process from a simple 'String' to a list of 'Rodin.Formula.Token' is done
in two major steps: 'Rodin.Formula.Tokenizer.explode', then 'Rodin.Formula.Tokenizer.extract'.

The purpose of 'Rodin.Formula.Tokenizer.explode' is to cut a string in tiny bits, @FTk@,
which are "pre-tokens" that will be identified and properly transformed by
'Rodin.Formula.Tokenizer.extract'.

Explode works by consuming characters one by one and forming @FTk@s along the way. Each
character is associated with a @Nature@. If this nature is compatible with the one of the
current @FTk@ being formed, they are simply merged. If not, the current @FTk@ is added to
the list of already formed @FTk@ and the current character serves as a base for the a new
@FTk@ under formation.

For example, a number alone or after anb operator will yield a new @Number@-natured @FTk@,
whereas, while parsing an identifier, this number is simply considered part of the token
currently being parsed. In other words:
        +0 => +[[O]], O[[N]]
        abc0 => abc0[[I]]

On a purely technical note, @FTk@ under formation or stored backward (because it is easier
to prepend) and reversed with 'Rodin.Formula.Tokenizer.reversef' when added to the accumulator
of tokens.
-}
explode :: String -> [FTk]
explode input =
    let (ftks,curr) = foldl eat ([],FTk [] Void 0) $ zip input [1..] in
        case ftknature curr of
          Void -> reverse ftks
          _    -> reverse ((reversef curr):ftks)
    where eat :: ([FTk],FTk) -> (Char,Int) -> ([FTk],FTk)
          eat (acc,curr) (c,pos) =
              let nat = ftknature curr
                  nat' = nature c in
                  case nat of
                    Void ->
                        (acc,FTk [c] nat' pos)
                    Number ->
                        case nat' of
                          Number -> (acc,c+:curr)
                          _      -> ((reversef curr):acc,FTk [c] nat' pos)
                    Ident ->
                        case nat' of
                          Ident  -> (acc,c+:curr)
                          Number -> (acc,c+:curr)
                          _      -> ((reversef curr):acc,FTk [c] nat' pos)
                    Operator ->
                        ((reversef curr):acc,FTk [c] nat' pos)
                    Space ->
                        ((reversef curr):acc,FTk [c] nat' pos)
                    Quote ->
                        case nat' of
                          Quote ->
                              let escaping = takeWhile (== '\\') $ ftkcontent curr in
                                  if (length escaping) `mod` 2 == 1 then
                                    (acc,c+:curr)
                                  else
                                    ((reversef (c+:curr)):acc, FTk [] Void pos)
                          _     -> (acc,c+:curr)

{-|
'Rodin.Formula.Tokenizer.extract' is the second step in building the list of tokens corresponding
to a given UTF8 sentence (after 'Rodin.Formula.Tokenizer.explode').

Its principle is to convert every @FTk@ previously built by 'Rodin.Formula.Tokenizer.explode' into
proper 'Rodin.Formula.Token'.

The function takes as argument a @printer@, that is a way to compare a candidate to available
tokens. This in theory makes the function fairly generic, as providing another printer allows
to parse other types of strings.

Note that the function (theoretically) also works with multi-characters operators. While
'Rodin.Formula.Tokenizer.explode' yields one @FTk@ by operator, 'Rodin.Formula.Tokenizer.extract' attempt
to aggregates as much @Operator@-natured @FTk@ and match them with existing operators. For instance:
        "-->" ==[explode]=> "-"[[O]],"-"[[O]],">"[[O]] ==[extract]=> "-->(TokOp TotalFunction)"
-}
extract :: (Token -> String) -> [FTk] -> ParseResult [Token]
extract printer tks =
    getTkns tks
    where getTkns :: [FTk] -> ParseResult [Token] -- Main function: parse FTk to guess tokens
          getTkns [] = return []                  -- No FTk means no token
          getTkns l@(tk:tks)                      -- We parse tokens one by one
            | ftknature tk == Operator =          -- We aggregate consecutive operators if possible
                let (op,rem) = span ((== Operator) . ftknature) l in
                    (++) <$> guessOps tk (reverse op) [] <*> getTkns rem
            | otherwise =                    -- Other tokens are theoretically already well-formed
                (++) <$> getOneTk tk <*> getTkns tks
          getOneTk :: FTk -> ParseResult [Token]  -- Transform one FTk to one or more Tokens
          getOneTk f@(FTk ct na po) 
            | na == Number   = return $ [TokIdent ct]
            | na == Space    = return $ [parseTk printer TokSpace SimpleSpace ct] -- guess space
            | na == Ident    = return $ [search ct] -- guess token
            | na == Quote    = -- Special case of quotes ("-delimited): eat tokens until next '"'
                if last ct /= '"' then
                  Left $ ParseError f UnclosedQuote
                else
                  let ct' = init $ tail $ ct in return [search ct']
            | na == Operator = -- Try to guess an operator
                case search ct of
                  TokIdent _ -> Left $ ParseError f $ UnexpectedOperator ct
                  tk         -> return [tk]
            | na == Void = return [] -- Weird case: nature-less FTk
          search :: String -> Token  -- Try to match a sequence to a possible Token
          search tk
            | isTk printer TokOp           Top         tk = parseTk printer TokOp           Top         tk
            | isTk printer TokSpace        SimpleSpace tk = parseTk printer TokSpace        SimpleSpace tk
            | isTk printer TokSpecialIdent Integers    tk = parseTk printer TokSpecialIdent Integers    tk
            | isTk printer TokToken        TokOpenPar  tk = parseTk printer TokToken        TokOpenPar  tk
            | otherwise = TokIdent tk -- Default case: correspond to nothing... this must be an identifier? Or it will be filtered out by calling function.
          --          Ctxt   Consid.  Remainder
          guessOps :: FTk -> [FTk] -> [FTk] -> ParseResult [Token] -- Guess a sequence of FTk
          guessOps ctx [] []  = return []
          guessOps ctx [] rem = Left $ ParseError ctx UnknownOperator
          guessOps ctx cs rem =
              let red = reduction $ reverse cs in
                case search $ ftkcontent red of
                  TokIdent id ->
                      guessOps ctx (tail cs) ((head cs):rem)
                  tk ->
                      let mop = guessOps ctx (reverse rem) [] in
                          if isLeft mop then guessOps ctx ((head rem):cs) (tail rem)
                          else (tk:) <$> mop
          reduction :: [FTk] -> FTk -- Basically flattens a list of FTk
          reduction r = (head r) { ftkcontent = foldl (\acc -> \x -> acc ++ (ftkcontent x)) "" r }
          -- Guess if current token candidate (tk) is a token of given family (elt) once passed
          -- in type constructor (fun) and in printer (printer)
          isTk :: (Enum a) => (Token -> String) -> (a -> Token) -> a -> String -> Bool
          isTk printer fun elt tk = tk `elem` (map (printer.fun) $ enumFrom elt)
          -- Same as isTk but actually yields the matching token
          parseTk :: (Enum a) => (Token -> String) -> (a -> Token) -> a -> String -> Token
          parseTk printer fun elt tk = fun $ fromJust $ lookup tk $ map (\x -> (printer $ fun x,x)) $ enumFrom elt

-- | Utility function for search-and-replacing HTML entities (@&...;@) with proper operators
-- This is mainly used when parsing XML.
replaceEntities :: String -> String
replaceEntities [] = []
replaceEntities s@(x:xs)
    | x == '&' =
        let (ent,rem) = span (/= ';') xs in
            if null rem
                then s
                else (replaceOne ent) ++ (replaceEntities (tail rem))
    | otherwise = x:(replaceEntities xs)
    where replaceOne "lt" = "<"
          replaceOne "gt" = ">"
          replaceOne "amp" = "&"
          replaceOne ('#':xs) = [chr $ read xs]

-- | Tokenize a string of characters into a formula using given printer
tokenize :: (Token -> String) -> String -> ParseResult Formula
tokenize printer = extract printer . explode

-- | Tokenize a string of characters into a formula using the default UTF8 printer
-- ('Rodin.Formula.UTF8'), discarding eventual errors. An erroneous formula yields
-- an empty formula
tkn :: String -> Formula
tkn str =
    case tokenize showUTF8 $ replaceEntities str of
      Left _ -> []
      Right l -> l




