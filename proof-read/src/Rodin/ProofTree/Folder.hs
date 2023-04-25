module Rodin.ProofTree.Folder where

import Rodin.Proofs (PrIdent)
import Rodin.ProofTree
import qualified Data.Map as M

data FoldState =
    A Antecedent | R Rule

-- Antecedent => name, hypothesis actions, added hypotheses, added identifiers, goal, accum
-- Rule => name, display, RID, confidence, goal, hypotheses, manual, accum
pt_foldup' :: (String -> [HypAction] -> [RefPred] -> [PrIdent] -> RefPred -> a -> a) -- Antecedent
           -> (String -> String -> String -> Int -> RefPred -> [RefPred] -> [a] -> a) -- Rule
           -> FoldState -- struct
           -> a
pt_foldup' whenAnte whenRule (A a) =
    let cue = pt_foldup' whenAnte whenRule $ R $ antecedentRule a in
        whenAnte (antecedentName a) (hypActions a) (addedHypotheses a) (addedIdentifiers a) (antecedentGoal a) cue
pt_foldup' whenAnte whenRule (R r) =
    let cue = map ((pt_foldup' whenAnte whenRule) . A) $ antecedents r in
        whenRule (ruleName r) (display r) (ruleRID r) (ruleConfidence r) (ruleGoal r) (hypotheses r) cue

-- Antecedent => name, hypothesis actions, added hypotheses, added identifiers, goal, accum
-- Rule => name, display, RID, confidence, goal, hypotheses, manual, accum
pt_foldup :: (String -> [HypAction] -> [RefPred] -> [PrIdent] -> RefPred -> a -> a)     -- Antecedent
          -> (String -> String -> String -> Int -> RefPred -> [RefPred] -> [a] -> a)    -- Rule
          -> (ProofName -> Int -> [PrIdent] -> RefPred -> [RefPred] -> Bool -> a -> a)  -- Proof itself
          -> ProofTree
          -> a
pt_foldup whenAnte whenRule whenProof pt =
    let cue = pt_foldup' whenAnte whenRule (R $ proofEntry pt) in
        whenProof (proofName pt) (proofConfidence pt) (proofIdents pt) (proofGoal pt) (proofHypotheses pt) (proofManual pt) cue


-- Antecedent => accum, name, hypothesis actions, added hypotheses, added identifiers, goal
-- Rule => accum, name, display, RID, confidence, goal, hypotheses, manual
pt_folddown' :: (a -> String -> [HypAction] -> [RefPred] -> [PrIdent] -> RefPred -> a) -- Antecedent
             -> (a -> String -> String -> String -> Int -> RefPred -> [RefPred] -> a) -- Rule
             -> a -- zero
             -> FoldState -- struct
             -> a
pt_folddown' whenAnte whenRule acc (A a) =
    let cue = whenAnte acc (antecedentName a) (hypActions a) (addedHypotheses a) (addedIdentifiers a) (antecedentGoal a) in
        pt_folddown' whenAnte whenRule cue (R $ antecedentRule a)
pt_folddown' whenAnte whenRule acc (R r) =
    let cue = whenRule acc (ruleName r) (display r) (ruleRID r) (ruleConfidence r) (ruleGoal r) (hypotheses r) in
        foldl (\acc' a -> pt_folddown' whenAnte whenRule acc' (A a)) cue $ antecedents r

-- Antecedent => accum, name, hypothesis actions, added hypotheses, added identifiers, goal
-- Rule => accum, name, display, RID, confidence, goal, hypotheses, manual
pt_folddown :: (a -> String -> [HypAction] -> [RefPred] -> [PrIdent] -> RefPred -> a) -- Antecedent
            -> (a -> String -> String -> String -> Int -> RefPred -> [RefPred] -> a) -- Rule
            -> (a -> ProofName -> Int -> [PrIdent] -> RefPred -> [RefPred] -> Bool -> a)  -- Proof itself
            -> a -- zero
            -> ProofTree
            -> a
pt_folddown whenAnte whenRule whenProof zero pt =
    let cue = whenProof zero (proofName pt) (proofConfidence pt) (proofIdents pt) (proofGoal pt) (proofHypotheses pt) (proofManual pt) in
        pt_folddown' whenAnte whenRule cue (R $ proofEntry pt)


size :: ProofTree -> Integer
size = pt_foldup (\_ _ _ _ _ s -> s) (\_ _ _ _ _ _ ss -> 1 + sum ss) (\_ _ _ _ _ _ s -> s)

depth :: ProofTree -> Integer
depth = pt_foldup (\_ _ _ _ _ s -> s) (\_ _ _ _ _ _ ss -> 1 + maximum ss) (\_ _ _ _ _ _ s -> s)

_inc :: (Ord k, Num v) => k -> M.Map k v -> M.Map k v
_inc k =
    M.insertWith (+) k 1

_merge :: (Ord k, Num v) => [M.Map k v] -> M.Map k v
_merge = M.unionsWith (+)

ruleApp :: ProofTree -> M.Map String Integer
ruleApp pt =
    pt_foldup (\_ _ _ _ _ s -> s) whenRule (\_ _ _ _ _ _ s -> s) pt
    where whenRule _ _ rid _ _ _ = (_inc rid) . _merge




