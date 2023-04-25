module Rodin.ProofTree where

import Rodin.Formula
import Rodin.Proofs (PrIdent)

data RefPred = RefPred { predicate_name :: String, predicate :: Formula }

data HypAction =
    SELECT      Int [RefPred]                     -- Target
  | DESELECT    Int [RefPred]
  | HIDE        Int [RefPred]
  | SHOW        Int [RefPred]
  | FORWARD_INF Int [RefPred] [RefPred]           -- Target, inferred
  | REWRITE     Int [RefPred] [RefPred] [RefPred] -- Target, inferred, hidden

data Antecedent = Antecedent {
    antecedentName :: String,
    hypActions :: [HypAction],
    addedHypotheses :: [RefPred],
    addedIdentifiers :: [PrIdent],
    antecedentGoal :: RefPred,
    antecedentRule :: Rule
}

data Rule = Rule {
    ruleName :: String,
    display :: String,
    ruleRID :: String,
    ruleConfidence :: Int,
    ruleGoal :: RefPred,
    hypotheses :: [RefPred],
    antecedents :: [Antecedent]
}

data ProofName = ProofName {
    proofSource :: String,
    proofType :: String,
    proofLabel :: Maybe String
} deriving Show

data ProofTree = ProofTree {
    proofName :: ProofName,
    proofConfidence :: Int,
    proofIdents :: [PrIdent],
    proofGoal :: RefPred,
    proofHypotheses :: [RefPred],
    proofManual :: Bool,
    proofEntry :: Rule
}




