module Rodin.ProofTree.Registry where

import qualified Data.Map as M
import qualified Rodin.Proofs as RP
import Rodin.ProofTree
import Rodin.Formula

type PRegistry = M.Map String Formula
type IRegistry = M.Map String Formula

data Registry = Registry {
    ireg :: M.Map String Formula,   -- Idents (val = type)
    preg :: M.Map String Formula,   -- Predicates (val = predicate)
    rreg :: M.Map String String     -- Proof rules (val = rule id)
}

emptyRegistry :: Registry
emptyRegistry = Registry { ireg = M.empty, preg = M.empty, rreg = M.empty }

addIdent :: Registry -> RP.PrIdent -> Registry
addIdent reg i =
    reg { ireg = M.insert (RP.pridName i) (RP.pridType i) (ireg reg) }

addPred :: Registry -> RP.PrPredicate -> Registry
addPred reg p =
    reg { preg = M.insert (RP.prName p) (RP.prPred p) (preg reg) }

addRule :: Registry -> RP.PrReas -> Registry
addRule reg r =
    reg { rreg = M.insert (RP.reasName r) (RP.reasRID r) (rreg reg) }

extractPred :: Registry -> RP.PrPredicate -> Registry
extractPred reg p =
    foldl addIdent (addPred reg p) (RP.prIdents p)

getIdent :: Registry -> String -> Maybe RP.PrIdent
getIdent reg s =
    case M.lookup s (ireg reg) of
      Nothing -> Nothing
      Just f -> Just $ RP.PrIdent s f

getPred :: Registry -> String -> Maybe RefPred
getPred reg s = RefPred s <$> M.lookup s (preg reg)

getRuleID :: Registry -> String -> Maybe String
getRuleID reg n = M.lookup n (rreg reg)

extractRegistry :: RP.Proof -> Registry
extractRegistry proof =
    let one = foldl addIdent emptyRegistry $ RP.proofIdentifiers proof in
        let two = foldl addRule one $ RP.proofReas proof in
            foldl extractPred two $ RP.proofPredicates proof


