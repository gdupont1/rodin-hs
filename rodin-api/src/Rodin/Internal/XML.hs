------------------------------------------------------------------------
-- File: Rodin/Internal/XML.hs - Part of rodinapi
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
Module      : Rodin.Internal.XML
Description : Utility functions for parsing XML
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

A few utility functions for dealing with XML files. This module relies on the
'Text.XML.Light' module.
-}
module Rodin.Internal.XML where

import Rodin.Formula (Formula)
import Rodin.Formula.Tokenizer (tokenize)
import Data.Either.Combinators (fromRight)
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)
import Data.List

-- | Namespace types
data EBNamespace = Core | TheoryCore deriving (Eq,Ord,Enum)

-- | Returns the namespace prefix corresponding to given namespace type
getPrefix :: EBNamespace -> String
getPrefix Core = "org.eventb.core."
getPrefix TheoryCore = "org.eventb.theory.core."

-- | Prepend a namespace qualification to given string, depending on given namespace type
pref :: EBNamespace -> String -> String
pref ns = (++) (getPrefix ns)


-- | Transform a list of XML attributes (attrname=value) to a list of pairs (attrname,val).
attrToTuple :: [Attr] -> [(String,String)]
attrToTuple = map (\x -> ((qName . attrKey) x, attrVal x))

-- | Look up for a value in a pair list (interpreted as a key-val structure) and return the
-- associated value, or the default value @def@ if the key does not exist.
lkOrDef :: Eq a => a -> [(a,b)] -> b -> b
lkOrDef elt lst def=
    case lookup elt lst of
      Just t -> t
      Nothing -> def

-- | Lookup for an attribute @a@ in a list of attributes @attrs@ and return its value or
-- the default value @def@ if no such attribute is present in the list.
lkOrDef' :: String -> [Attr] -> String -> String
lkOrDef' a attrs def = lkOrDef a (attrToTuple attrs) def

-- | Tests if the given qualified name of the XML element matches @name@
isQName :: String -> Element -> Bool
isQName name = (== name) . qName . elName

-- | Tests if the given qualified name of the XML element matches the given
-- name @name@ with associated prefix function @pr@.
isQName' :: (String -> String) -> String -> Element -> Bool
isQName' pr name = isQName $ pr name

-- | Filter the children of given element using filter function @filt@ and parse
-- each of these children using the parsing function @fun@.
parseChildren :: (Element -> Bool) -> (Element -> a) -> Element -> [a]
parseChildren filt fun =
    (map fun') . (filter filt') . elContent
    where filt' (Elem elt) = filt elt
          filt' _ = False
          fun' (Elem elt) = fun elt




