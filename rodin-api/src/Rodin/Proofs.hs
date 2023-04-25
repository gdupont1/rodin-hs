------------------------------------------------------------------------
-- File: Rodin/Proofs.hs - Part of rodinapi
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
Module      : Rodin.Proofs
Description : Description of a proof file in Rodin
Copyright   : Copyright (C) 2023 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This describes a Rodin proof file, that contains the steps of the proofs
of various POs.
Other submodules are responsible for other features.
-}
module Rodin.Proofs where

import Rodin.Formula (Formula)

-- | A rule application, in an attempt to transform the given goal, progressing
-- towards the given antecedents.
--
-- Rules with no antecedents are simply terminal nodes (|- T, H |- H, etc.)
data PrRule = PrRule {
    ruleName :: String,                 -- ^ Rule "name" (ID) used
    ruleConfidence :: Int,              -- ^ Confidence score for the rule
    ruleDisplay :: String,              -- ^ Human redable description of the rule
    ruleHypotheses :: [String],         -- ^ Set of added hypotheses
    ruleGoal :: Maybe String,           -- ^ Rule goal (if Nothing : same as parent)
    ruleAnte :: [PrAnte]                -- ^ Rule antecedents (= next proof branches)
}

-- | A rule antecedent, i.e. the result of a rule application, to be transformed 
-- by another rule (in an Ante/Rule alternation)
data PrAnte = PrAnte {
    anteName :: String,                 -- ^ Antecedent name
    anteHypotheses :: [String],         -- ^ Hypotheses added in the antecedent (by the rule)
    anteGoal :: Maybe String,           -- ^ Antecedent goal (if Nothing : same as parent)
    anteHypActions :: [PrHypAction],    -- ^ List of "hypotheses actions"
    anteIdents :: [PrIdent],            -- ^ Identifiers added in the antecedent (by the rule)
    anteRule :: PrRule                  -- ^ Rule applied on the antecedent to progress the proof further
}

-- | An "hypothesis action", i.e. a simple operation on hypotheses representing evolution
-- of the hypothesis set.
--
-- hypActions' names typically contains the actual action performed (see ProofTree)
data PrHypAction = PrHypAction {
    haName :: String,                   -- ^ HypAction name/ID
    haHidden :: [String],               -- ^ Set of hypotheses hidden by the action
    haHypotheses :: [String],           -- ^ Set of hypotheses added by the action
    haInfHypotheses :: [String]         -- ^ Set of hypotheses added by inference by the action
}

-- | An identifier with a type.
data PrIdent = PrIdent {
    pridName :: String,                 -- ^ Identifier
    pridType :: Formula                 -- ^ Identifier's type
}

-- | A predicate, with a name and a set of identifiers
data PrPredicate = PrPredicate {
    prName :: String,                   -- ^ Predicate name (serving as ID for cross ref)
    prPred :: Formula,                  -- ^ Predicate formula
    prIdents :: [PrIdent]               -- ^ Identifiers involved in the predicates
}

-- | Rule definitions, featuring name and actual "tool" ID
data PrReas = PrReas {
    reasName :: String,                 -- ^ Rule "name" (ID) as encountered in proofs
    reasRID :: String                   -- ^ Associated tool ID
}

-- | A proof
data Proof = Proof {
    proofName :: String,                -- ^ Proof name, which is in fact the PO name
    proofConfidence :: Int,             -- ^ Confidence score for the PO
    proofFresh :: [String],             -- ^ Fresh variables for the PO (from quantification)
    proofGoal :: String,                -- ^ Proof goal
    proofHypotheses :: [String],        -- ^ Specific/local hypotheses
    proofManual :: Bool,                -- ^ Flag indicating that the proof is manual or not
    proofEntry :: PrRule,               -- ^ First rule application on the goal
    proofPredicates :: [PrPredicate],   -- ^ Dictionary of predicates for the proof (cross-ref'd in the tree)
    proofIdentifiers :: [PrIdent],      -- ^ Dictionary of identifiers + type for the proof (cross-ref'd in the tree)
    proofReas :: [PrReas]               -- ^ List of tools used with associated name/ID for cross-ref
}

-- | A proof file (.bpr), essentially a list of proofs
data ProofFile = ProofFile {
    pfName :: String,                   -- ^ File name
    pfProofs :: [Proof]                 -- ^ Proofs in the file
}






