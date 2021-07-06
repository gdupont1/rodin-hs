------------------------------------------------------------------------
-- File: Rodin/Internal/Util.hs - Part of rodinapi
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
Module      : Rodin.Internal.Util
Description : Small uiliy module for Rodin API
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module only contains a few utility functions.
-}
module Rodin.Internal.Util where

import Data.List (intercalate)

-- | Utility for splitting a list every time an element fulfill a given predicate
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p = foldr (\x -> \l@(h:t) -> if p x then []:l else (x:h):t) [[]]

-- | Repeat @n@ times the string @s@.
indent_ :: String -> Int -> String
indent_ s n = concat $ take n $ repeat s

-- | Generate @n@ level of identation. One level of identation consists of 2 spaces.
ind :: Int -> String
ind = indent_ "  "

-- | Prepent and append some strings to a string. This is typically useful with parentheses
-- and other delimiters. For instance:
--          @circle "(" ")" "test" = "(test)"
circle :: String -> String -> String -> String
circle before after = ((++) before) . (++ after)

-- | Print a list of element as one string using given printer
printlist :: (a -> String) -> String -> [a] -> String
printlist printer c = intercalate c . map printer

-- | Print comments if they are present
printcomment :: Maybe String -> String
printcomment Nothing = ""
printcomment (Just c) = " -- " ++ c

-- | Extract the name of a component from its filename.
-- Note: Rodin does not store the name of a component inside its file; it actually
-- "guesses" it from its filename ('/foo/bar/Machine.bum' => Machine). This function
-- guesses (supposedly) the same thing as Rodin.
getRodinFileName :: String -> String
getRodinFileName =
    reverse . takeWhile continue . tail . dropWhile (/= '.') . reverse
    where continue = (&&) <$> (/= '/') <*> (/= '\\')



