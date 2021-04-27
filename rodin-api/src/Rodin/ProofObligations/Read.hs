------------------------------------------------------------------------
-- File: Rodin/ProofObligations/Read.hs - Part of rodinapi
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
Module      : Rodin.ProofObligations.Read
Description : Rodin PO file reader
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

Module for reading a Rodin PO file (.bpo) as an XML File.
-}
module Rodin.ProofObligations.Read where

import Rodin.Formula (Formula)
import Rodin.Formula.Tokenizer (tkn)
import Rodin.ProofObligations
import Rodin.Internal.XML
import Rodin.Internal.Util (getRodinFileName)
import Data.Either.Combinators (fromRight)
import Data.List
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)

-- | Parse an identifier
parsePOIdentifier :: Element -> POIdentifier
parsePOIdentifier elt =
    POIdentifier {
        poiName =       lkOrDef            "name"  attrskv "",
        poiType = tkn $ lkOrDef (pref Core "type") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a predicate
parsePOPredicate :: Element -> POPredicate
parsePOPredicate elt =
    POPredicate {
        popName      =       lkOrDef            "name"       attrskv "",
        popPredicate = tkn $ lkOrDef (pref Core "predicate") attrskv "",
        popSource    =       lkOrDef (pref Core "source")    attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a predicate set
parsePOPredicateSet :: Element -> POPredicateSet
parsePOPredicateSet elt =
    POPredicateSet {
        popsName        = lkOrDef            "name"       attrskv "",
        popsParentSet   = lkOrDef (pref Core "parentSet") attrskv "",
        popsStamp       = lkOrDef (pref Core "poStamp"  ) attrskv "",
        popsIdentifiers = parseChildren (isQName $ pref Core "poIdentifier") parsePOIdentifier elt,
        popsPredicates  = parseChildren (isQName $ pref Core "poPredicate" ) parsePOPredicate  elt
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a source reference
parsePOSource :: Element -> POSource
parsePOSource elt = POSource {
        posrcName   = lkOrDef            "name"    attrskv "",
        posrcRole   = lkOrDef (pref Core "poRole") attrskv "",
        posrcSource = lkOrDef (pref Core "source") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a hint
parsePOSelHint :: Element -> POSelHint 
parsePOSelHint elt = POSelHint {
        poshName   = lkOrDef            "name"          attrskv "",
        poshFirst  = lkOrDef (pref Core "poSelHintFst") attrskv "",
        poshSecond = lkOrDef (pref Core "poSelHintSnd") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a sequent
parsePOSequent :: Element -> POSequent
parsePOSequent elt = POSequent {
        posName          =  lkOrDef            "name"      attrskv "",
        posAccurate      = (lkOrDef (pref Core "accurate") attrskv "false") == "true",
        posDesc          =  lkOrDef (pref Core "poDesc"  ) attrskv "",
        posStamp         =  lkOrDef (pref Core "poStamp" ) attrskv "",
        posPredicateSets = parseChildren (isQName $ pref Core "poPredicateSet") parsePOPredicateSet elt,
        posPredicates    = parseChildren (isQName $ pref Core "poPredicate"   ) parsePOPredicate    elt,
        posSources       = parseChildren (isQName $ pref Core "poSource"      ) parsePOSource       elt,
        posSelHints      = parseChildren (isQName $ pref Core "poSelHint"     ) parsePOSelHint      elt
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a set of PO
parsePOFile :: String -> Element -> POFile
parsePOFile name elt = POFile {
        pofName          = name,
        pofStamp         = lkOrDef (pref Core "poStamp" ) attrskv "",
        pofPredicateSets = parseChildren (isQName $ pref Core "poPredicateSet") parsePOPredicateSet elt,
        pofSequents      = parseChildren (isQName $ pref Core "poSequent"     ) parsePOSequent      elt
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a PO file from a filename
parsePOFileFile' :: String -> IO POFile
parsePOFileFile' filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ POFile "" "" [] [])
          Just elt -> return $ parsePOFile (getRodinFileName filename) elt)

parsePOFileFile :: String -> IO (Maybe POFile)
parsePOFileFile filename =
    readFile filename >>= (\str -> return $ (parsePOFile (getRodinFileName filename)) <$> parseXMLDoc str)


