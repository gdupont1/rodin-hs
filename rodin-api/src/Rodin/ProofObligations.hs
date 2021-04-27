------------------------------------------------------------------------
-- File: Rodin/ProofObligations.hs - Part of rodinapi
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
Module      : Rodin.ProofObligations
Description : Description of a proof obligation (PO) file in Rodin
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This describes a Rodin PO file, holding sequents that must be
proven.
Other submodules are responsible for other features.
-}
module Rodin.ProofObligations where

import Rodin.Formula (Formula)

-- | An identifier in a proof obligation (a variable with a type)
data POIdentifier = POIdentifier {
    poiName :: String,                          -- ^ Identifier name
    poiType :: Formula                          -- ^ Identifier type
}

-- | A predicate, used to represent the goal or an hypothesis of a sequent
data POPredicate = POPredicate {
    popName :: String,                          -- ^ Predicate name
    popPredicate :: Formula,                    -- ^ Predicate formula
    popSource :: String                         -- ^ Predicate source (machine, element)
}

-- | A set of predicates, used to represent a set of hypotheses typically
data POPredicateSet = POPredicateSet {
    popsName :: String,                         -- ^ Set name
    popsParentSet :: String,                    -- ^ Parent set (ref. to another set of predicates, for instance the general hypotheses of a component)
    popsStamp :: String,                        -- ^ Set stamp
    popsIdentifiers :: [POIdentifier],          -- ^ Identifiers of the predicate set, used by the predicates
    popsPredicates :: [POPredicate]             -- ^ Predicates of the set
}

-- | Denotes a source, generally for a set of hypotheses
data POSource = POSource {
    posrcName :: String,                        -- ^ Source name
    posrcRole :: String,                        -- ^ Source role
    posrcSource :: String                       -- ^ Reference to the source
}

-- | Hint for solving the PO (?) (not sure what this does actually)
data POSelHint = POSelHint {
    poshName :: String,                         -- ^ Hint name
    poshFirst :: String,                        -- ^ Hint first member (LHS?)
    poshSecond :: String                        -- ^ Hint second member (RHS?)
}

-- | A proof obligation as a sequent
data POSequent = POSequent {
    posName :: String,                          -- ^ Sequent name
    posAccurate :: Bool,                        -- ^ Accuracy (not sure what it is, always true in what I saw)
    posDesc :: String,                          -- ^ Sequent description
    posStamp :: String,                         -- ^ Sequent stamp
    posPredicateSets :: [POPredicateSet],       -- ^ Sequent hypotheses as a predicate set
    posPredicates :: [POPredicate],             -- ^ Sequent goal
    posSources :: [POSource],                   -- ^ Sources of the sequent
    posSelHints :: [POSelHint]                  -- ^ Hints for the sequent
}

-- | A proof obligation file
data POFile = POFile {
   pofName :: String,                           -- ^ PO file name (and subsequently linked component)
   pofStamp :: String,                          -- ^ PO file stamp
   pofPredicateSets :: [POPredicateSet],        -- ^ Global hypotheses as a predicate set
   pofSequents :: [POSequent]                   -- ^ List of POs as sequents
}


