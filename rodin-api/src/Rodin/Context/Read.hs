------------------------------------------------------------------------
-- File: Rodin/Context/Read.hs - Part of rodinapi
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
Module      : RodinContext.Read
Description : Rodin Context XML file reader
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

Module for reading a Rodin context (.buc) as an XML File.
-}
module Rodin.Context.Read (
        parseContextFile
    ) where

import Rodin.Formula (Formula)
import Rodin.Formula.Tokenizer (tkn)
import Rodin.Internal.XML
import Rodin.Internal.Util (getRodinFileName)
import Rodin.Context
import Data.Either (partitionEithers)
import Data.Either.Combinators (fromRight)
import Data.List
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)

-- | Parse an @extendsContext@ XML element, holding a 'Rodin.Context.ExtendsContext' relationship
parseExtendsContext :: Element -> ExtendsContext
parseExtendsContext elt =
    ExtendsContext {
        ecTarget = lkOrDef (pref Core "target") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an @axiom@ XML element, holding an 'Rodin.Context.Axiom' element.
-- Such elements represent both axioms and theorems (as decided by the `theorem`
-- flag), but in the type, elements are separate.
parseAxmThm :: Element -> Either Axiom Theorem
parseAxmThm elt =
    if isThm == "true"
        then Right $ Theorem { thLabel = label, thPred = pred }
        else Left  $ Axiom   { axLabel = label, axPred = pred }
    where attrskv   = attrToTuple $ elAttribs elt
          isThm     = lkOrDef (pref Core "theorem") attrskv ""
          label     =       lkOrDef (pref Core "label")     attrskv ""
          pred      = tkn $ lkOrDef (pref Core "predicate") attrskv ""

-- | Parse a @constant@ XML element, holding a 'Rodin.Context.Constant' element
parseConstant :: Element -> Constant
parseConstant elt =
    Constant {
        ctName =       lkOrDef (pref Core "identifier") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a @carrierSet@ XML element, holding a 'Rodin.Context.CarrierSet' element
parseCarrierSet :: Element -> CarrierSet
parseCarrierSet elt =
    CarrierSet {
        csName =       lkOrDef (pref Core "identifier") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a @contextFile@ XML element (root of a context file in Rodin), holding a 'Rodin.Context.Context' element
parseContext :: String -> Element -> Context
parseContext name elt =
    Context {
        ctxName      = name,
        ctxExtends   = parseChildren (isQName $ pref Core "extendsContext") parseExtendsContext elt,
        ctxAxioms    = axms,
        ctxTheorems  = thms,
        ctxConstants = parseChildren (isQName $ pref Core "constant"      ) parseConstant       elt,
        ctxSets      = parseChildren (isQName $ pref Core "carrierSet"    ) parseCarrierSet     elt
    }
    where axAndTh = parseChildren (isQName $ pref Core "axiom") parseAxmThm elt
          (axms, thms) = partitionEithers axAndTh

-- | Read a context file denoted by @filename@ and extract the 'Rodin.Context.Context' from its content
parseContextFile' :: String -> IO Context
parseContextFile' filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ Context "??" [] [] [] [] [])
          Just elt -> return $ parseContext (getRodinFileName filename) elt)


parseContextFile :: String -> IO (Maybe Context)
parseContextFile filename =
    readFile filename >>= (\str -> return $ (parseContext (getRodinFileName filename)) <$> parseXMLDoc str)


