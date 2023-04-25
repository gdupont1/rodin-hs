------------------------------------------------------------------------
-- File: Rodin/Proofs/Read.hs - Part of rodinapi
------------------------------------------------------------------------
-- Copyright (C) 2023  G. Dupont
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
Module      : Rodin.Proofs.Read
Description : Rodin proof file reader
Copyright   : Copyright (C) 2023 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

Module for reading a Rodin proof file (.bpr) as an XML File.
-}
module Rodin.Proofs.Read where

import Rodin.Formula (Formula)
import Rodin.Formula.Tokenizer (tkn)
import Rodin.Proofs
import Rodin.Internal.XML
import Rodin.Internal.Util (getRodinFileName)
import Data.Either.Combinators (fromRight)
import Data.List
import Data.List.Split
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)

purge :: [String] -> [String]
purge = filter (not . null)

parseRule :: Element -> PrRule
parseRule elt =
    PrRule {
        ruleName       =                       lkOrDef "name"                   attrskv "",
        ruleConfidence =         read $        lkOrDef (pref Core "confidence") attrskv "-1",
        ruleDisplay    =                       lkOrDef (pref Core "prDisplay")  attrskv "",
        ruleHypotheses = purge $ splitOn "," $ lkOrDef (pref Core "prHyps")     attrskv "",
        ruleGoal       =                       lookup  (pref Core "prGoal")     attrskv,
        ruleAnte       = parseChildren (isQName $ pref Core "prAnte") parseAnte elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parseAnte :: Element -> PrAnte
parseAnte elt =
    PrAnte {
        anteName       =                       lkOrDef "name"               attrskv "",
        anteHypotheses = purge $ splitOn "," $ lkOrDef (pref Core "prHyps") attrskv "",
        anteGoal       =                       lookup  (pref Core "prGoal") attrskv,
        anteHypActions =        parseChildren (isQName $ pref Core "prHypAction") parseHypAction elt,
        anteIdents     =        parseChildren (isQName $ pref Core "prIdent")     parseIdent     elt,
        anteRule       = head $ parseChildren (isQName $ pref Core "prRule")      parseRule      elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parseHypAction :: Element -> PrHypAction
parseHypAction elt =
    PrHypAction {
        haName          =                       lkOrDef "name"                  attrskv "",
        haHidden        = purge $ splitOn "," $ lkOrDef (pref Core "prHidden")  attrskv "",
        haHypotheses    = purge $ splitOn "," $ lkOrDef (pref Core "prHyps")    attrskv "",
        haInfHypotheses = purge $ splitOn "," $ lkOrDef (pref Core "prInfHyps") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseIdent :: Element -> PrIdent
parseIdent elt =
    PrIdent {
        pridName =       lkOrDef "name"             attrskv "",
        pridType = tkn $ lkOrDef (pref Core "type") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parsePredicate :: Element -> PrPredicate
parsePredicate elt =
    PrPredicate {
        prName   =       lkOrDef "name"                  attrskv "",
        prPred   = tkn $ lkOrDef (pref Core "predicate") attrskv "",
        prIdents = parseChildren (isQName $ pref Core "prIdent") parseIdent elt
    }
    where attrskv = attrToTuple $ elAttribs elt

parseReas :: Element -> PrReas
parseReas elt =
    PrReas {
        reasName = lkOrDef "name"              attrskv "",
        reasRID  = lkOrDef (pref Core "prRID") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

parseProof :: Element -> Proof
parseProof elt =
    Proof {
        proofName        =                       lkOrDef "name"                   attrskv "",
        proofConfidence  =         read        $ lkOrDef (pref Core "confidence") attrskv "0",
        proofFresh       = purge $ splitOn "," $ lkOrDef (pref Core "prFresh")    attrskv "",
        proofGoal        =                       lkOrDef (pref Core "prGoal")     attrskv "",
        proofHypotheses  = purge $ splitOn "," $ lkOrDef (pref Core "prHyps")     attrskv "",
        proofManual      =         rdbool      $ lkOrDef (pref Core "prManual")   attrskv "",
        proofEntry       = head $ parseChildren (isQName $ pref Core "prRule")  parseRule      elt,
        proofPredicates  =        parseChildren (isQName $ pref Core "prPred")  parsePredicate elt,
        proofIdentifiers =        parseChildren (isQName $ pref Core "prIdent") parseIdent     elt,
        proofReas        =        parseChildren (isQName $ pref Core "prReas")  parseReas      elt
    }
    where attrskv = attrToTuple $ elAttribs elt
          rdbool "true" = True
          rdbool _      = False

parseProofFile :: String -> Element -> ProofFile
parseProofFile name elt =
    ProofFile {
        pfName = name,
        pfProofs = parseChildren (isQName $ pref Core "prProof") parseProof elt
    }

parseProofFileFile' :: String -> IO ProofFile
parseProofFileFile' filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ ProofFile "" [])
          Just elt -> return $ parseProofFile (getRodinFileName filename) elt)

parseProofFileFile :: String -> IO (Maybe ProofFile)
parseProofFileFile filename =
    readFile filename >>= (\str -> return $ (parseProofFile (getRodinFileName filename)) <$> parseXMLDoc str)




