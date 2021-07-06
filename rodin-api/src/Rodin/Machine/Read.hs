------------------------------------------------------------------------
-- File: Rodin/Machine/Read.hs - Part of rodinapi
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
Module      : Rodin.Machine.Read
Description : Module for reading Rodin machine XML files
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

Module for parsing a Rodin machine XML (.bum) file to a 'Rodin.Machine.Machine'.
-}
module Rodin.Machine.Read (
        parseMachineFile
    ) where

import Rodin.Machine
import Rodin.Formula (Formula)
import Rodin.Formula.Tokenizer (tkn)
import Rodin.Internal.XML
import Rodin.Internal.Util (getRodinFileName)
import Data.Either.Combinators (fromRight)
import Data.List
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)

-- | Parse a refinement relationship
parseRefinesMachine :: Element -> RefinesMachine
parseRefinesMachine elt =
    RefinesMachine {
        rmTarget = lkOrDef (pref Core "target") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a context seeing relationship
parseSeesContext :: Element -> SeesContext
parseSeesContext elt =
    SeesContext {
        scTarget = lkOrDef (pref Core "target") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a machine variable
parseVariable :: Element -> Variable
parseVariable elt =
    Variable {
        vaIdentifier = tkn $ lkOrDef (pref Core "identifier") attrskv "",
        vaComment    =       lookup  (pref Core "comment")    attrskv
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a machine invariant
parseInvariant :: Element -> Invariant
parseInvariant elt =
    Invariant {
        invLabel   =       lkOrDef (pref Core "label"    ) attrskv "",
        invPred    = tkn $ lkOrDef (pref Core "predicate") attrskv "",
        invComment =       lookup  (pref Core "comment"  ) attrskv
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a machine variant
parseVariant :: Element -> Variant
parseVariant elt =
    Variant {
        varExpression = tkn $ lkOrDef (pref Core "expression") attrskv "",
        varComment    =       lookup  (pref Core "comment"   ) attrskv
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an event refinement relationship
parseRefinesEvent :: Element -> RefinesEvent
parseRefinesEvent elt =
    RefinesEvent {
        reTarget = lkOrDef (pref Core "target") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an event parameter
parseParameter :: Element -> Parameter
parseParameter elt =
    Parameter {
        paIdentifier = tkn $ lkOrDef (pref Core "identifier") attrskv "",
        paComment    =       lookup  (pref Core "comment")    attrskv
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an event guard
parseGuard :: Element -> Guard
parseGuard elt =
    Guard {
        guLabel   =       lkOrDef (pref Core "label"    ) attrskv "",
        guPred    = tkn $ lkOrDef (pref Core "predicate") attrskv "",
        guComment =       lookup  (pref Core "comment"  ) attrskv
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an event witness
parseWitness :: Element -> Witness
parseWitness elt =
    Witness {
        wiLabel   = tkn $ lkOrDef (pref Core "label"    ) attrskv "",
        wiPred    = tkn $ lkOrDef (pref Core "predicate") attrskv "",
        wiComment =       lookup  (pref Core "comment"  ) attrskv
    } 
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an event action
parseAction :: Element -> Action
parseAction elt =
    Action {
        acLabel      =       lkOrDef (pref Core "label"     ) attrskv "",
        acAssignment = tkn $ lkOrDef (pref Core "assignment") attrskv "",
        acComment    =       lookup  (pref Core "comment"   ) attrskv
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a machine event
parseEvent :: Element -> Event
parseEvent elt =
    Event {
        evLabel       =                 lkOrDef (pref Core "label"       ) attrskv "",
        evConvergence = toEnum $ read $ lkOrDef (pref Core "convergence" ) attrskv "",
        evExtended    =                (lkOrDef (pref Core "extended"    ) attrskv "false") == "true",
        evRefines     = parseChildren (isQName $ pref Core "refinesEvent") parseRefinesEvent elt,
        evParameters  = parseChildren (isQName $ pref Core "parameter"   ) parseParameter    elt,
        evGuards      = parseChildren (isQName $ pref Core "guard"       ) parseGuard        elt,
        evWitnesses   = parseChildren (isQName $ pref Core "witness"     ) parseWitness      elt,
        evActions     = parseChildren (isQName $ pref Core "action"      ) parseAction       elt,
        evComment     =                 lookup  (pref Core "comment"     ) attrskv
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a machine
parseMachine :: String -> Element -> Machine
parseMachine name elt =
    Machine {
        maName       = name,
        maRefines    = parseChildren (isQName $ pref Core "refinesMachine") parseRefinesMachine elt,
        maSeesCtx    = parseChildren (isQName $ pref Core "seesContext"   ) parseSeesContext    elt,
        maVariables  = parseChildren (isQName $ pref Core "variable"      ) parseVariable       elt,
        maInvariants = parseChildren (isQName $ pref Core "invariant"     ) parseInvariant      elt,
        maVariants   = parseChildren (isQName $ pref Core "variant"       ) parseVariant        elt,
        maEvents     = parseChildren (isQName $ pref Core "event"         ) parseEvent          elt,
        maComment    = lookup (pref Core "comment") attrskv
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a machine (.bum) XML file to a 'Rodin.Machine.Machine'
parseMachineFile' :: String -> IO Machine
parseMachineFile' filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ Machine "??" [] [] [] [] [] [] Nothing)
          Just elt -> return $ parseMachine (getRodinFileName filename) elt)

parseMachineFile :: String -> IO (Maybe Machine)
parseMachineFile filename =
    readFile filename >>= (\str -> return $ (parseMachine (getRodinFileName filename)) <$> parseXMLDoc str)





