------------------------------------------------------------------------
-- File: Rodin/Formula/Internal/Util.hs - Part of rodinapi
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
Module      : Formula.Util
Description : utility for dealing with formulas
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

Module with utility functions for dealing with formulas.
-}
module Rodin.Formula.Internal.Util where

import Rodin.Internal.Util
import Rodin.Formula

-- | Is the token a "math" symbol (in the LaTeX sense)
isMath :: Token -> Bool
isMath (TokOp _) = True
isMath (TokSpecialIdent _) = True
isMath (TokToken TokDot) = True
isMath (TokToken TokMid) = True
isMath _ = False

-- | Is the token a space
isSpace :: Token -> Bool
isSpace (TokSpace _) = True
isSpace _ = False

-- | Is the operator "function-like" (i.e. to be used as @<op>(<arg1>,<arg2>,...)@)
functionLike :: Operator -> Bool
functionLike =
    (flip elem) [
      Not
    , Powerset
    , Powerset1
    , GenUnion
    , GenIntersection
    , Inverse
    , Lambda
    ]

-- | Is the token the a new line
isNewline :: Token -> Bool
isNewline (TokSpace Newline) = True
isNewline _ = False

-- | Print a list of tokens as multiple lines (each element of the resulting list being
-- one of this line). Each line is printed using a given printer ('printLine') allowing
-- for uniform indenting for example.
printLines' :: ([Token] -> String) -> [Token] -> [String]
printLines' printLine tks =
    let tkss = splitWhen isNewline tks in
        map printLine tkss

-- | Print a list of tokens as multiple lines, aggregating into one string, using
-- a special line priner ('printLine') and indenting every line with the given
-- number of space ('indet')
printLines :: ([Token] -> String) -> Int -> [Token] -> String
printLines printLine indent =
    foldl (\acc -> (++) $ acc ++ "\n" ++ ind indent) "" . (printLines' printLine)

{-| The following functions are mainly utilities for taking out typing information
from formulas (especially proof obligations).
-}
pruneType :: ([Token],[Token]) -> ([Token],[Token])
pruneType (acc,[]) = (acc,[])
pruneType (acc,(x:xs)) =
    case x of
      TokSpace _             -> pruneType (acc++[x],xs)
      TokOp Powerset         -> pruneType (acc++[x],xs)
      TokToken TokOpenPar    -> pruneType $ takeOutParentheses (acc++[x],xs)
      TokIdent _             -> pruneType (acc++[x],xs)
      TokOp CartesianProduct -> pruneType (acc++[x],xs)
      _                      -> (acc,x:xs)

-- | Remove a pair of opening-closing parentheses
takeOutParentheses :: ([Token],[Token]) -> ([Token],[Token])
takeOutParentheses (acc,[]) = (acc,[])
takeOutParentheses (acc,(x:xs)) =
    case x of
      TokToken TokOpenPar  -> let (acc',xs') = takeOutParentheses ([x],xs) in takeOutParentheses (acc ++ acc', xs')
      TokToken TokClosePar -> (acc ++ [x], xs)
      _                    -> takeOutParentheses (acc ++ [x], xs)

-- | Remove type anotations from a formula
untype :: [Token] -> [Token]
untype [] = []
untype ((TokSpace _):(TokOp OfType):xs) = untype ((TokOp OfType):xs)
untype ((TokOp OfType):xs) =
    let (acc,xs') = pruneType ([],xs)
      in untype xs'
untype (x:xs) = x:(untype xs)

-- | Remove (most of) unneeded parentheses around identifiers (such as ((3)+(5)) or (f)(...))
removeAloneParentheses :: [Token] -> [Token]
removeAloneParentheses tks =
    aux Nothing tks
    where aux _ [] = []
          aux Nothing ((TokToken TokOpenPar):i:(TokToken TokClosePar):xs) = i:(aux Nothing xs)
          aux (Just (TokOp o)) l@((TokToken TokOpenPar):i:(TokToken TokClosePar):xs)
              | not $ functionLike o = i:(aux Nothing xs)
              | otherwise = (head l):(aux (Just $ head l) (tail l))
          aux (Just (TokSpace _)) l@((TokToken TokOpenPar):i:(TokToken TokClosePar):xs) =
              i:(aux Nothing xs)
          aux (Just (TokToken x)) l@((TokToken TokOpenPar):i:(TokToken TokClosePar):xs)
              | x /= TokClosePar = i:(aux Nothing xs)
              | otherwise = (head l):(aux (Just $ head l) (tail l))
          aux _ (x:xs) = x:(aux (Just x) xs)




