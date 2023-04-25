module Rodin.ProofTree.Arrange where

import qualified Rodin.Proofs as RP
import Rodin.ProofTree
import Rodin.ProofTree.Registry
import Rodin.ProofTree.Util
import Rodin.Formula

import Control.Monad

justRight :: String -> Maybe a -> Either String a
justRight e Nothing = Left e
justRight _ (Just x) = Right x

justRightSeq :: String -> [Maybe a] -> Either String [a]
justRightSeq _ [] = Right []
justRightSeq e (Nothing:_) = Left e
justRightSeq e ((Just x):xs) = (:) <$> (Right x) <*> (justRightSeq e xs)

justRightMap :: (a -> String) -> (a -> Maybe b) -> [a] -> Either String [b]
justRightMap _ _ [] = Right []
justRightMap e f (x:xs) =
    case f x of
      Nothing -> Left $ e x
      Just y -> (:) <$> (Right y) <*> (justRightMap e f xs)

mkAnte :: Registry -> Maybe RefPred -> RP.PrAnte -> Either String Antecedent
mkAnte reg lastGoal ante = do
    hs <- justRightMap (\x -> "hypothesis " ++ x ++ " undefined in antecedent " ++ name) (getPred reg) $ RP.anteHypotheses ante
    g <- justRight ("goal " ++ (show $ RP.anteGoal ante) ++ " for antecedent " ++ name ++ " is undefined ") goal
    ha <- justRightMap (\x -> "cannot parse hypothesis action " ++ RP.haName x ++ " in antecedent " ++ name) (parseHA reg) $ RP.anteHypActions ante
    rl <- rule
    return $ Antecedent {
        antecedentName = name,
        hypActions = ha,
        addedHypotheses = hs,
        addedIdentifiers = adid,
        antecedentGoal = g,
        antecedentRule = rl
    }
    where name = RP.anteName ante
          goal = 
              case RP.anteGoal ante of
                Nothing -> lastGoal
                Just x -> getPred reg x
          hact = sequence $ map (parseHA reg) $ RP.anteHypActions ante
          adid = RP.anteIdents ante
          reg' = foldl addIdent reg adid
          rule = mkRule reg' goal $ RP.anteRule ante

mkRule :: Registry -> Maybe RefPred -> RP.PrRule -> Either String Rule
mkRule reg lastGoal rule = do
    ri <- justRight ("Undefined rule ID for rule " ++ name) rid
    g <- justRight ("goal " ++ (show $ RP.ruleGoal rule) ++ " for antecedent " ++ name ++ " is undefined ") goal
    hs <- justRightMap (\x -> "hypothesis " ++ x ++ " undefined in antecedent " ++ name) (getPred reg) $ RP.ruleHypotheses rule
    as <- ante
    return $ Rule {
        ruleName = name,
        ruleRID = ri,
        ruleGoal = g,
        hypotheses = hs,
        ruleConfidence = rcon,
        display = disp,
        antecedents = as
    }
    where name = RP.ruleName rule
          disp = RP.ruleDisplay rule
          rid  = getRuleID reg name
          rcon = RP.ruleConfidence rule
          goal =
              case RP.ruleGoal rule of
                Nothing -> lastGoal
                Just x -> getPred reg x
          ante = sequence $ map (mkAnte reg goal) $ RP.ruleAnte rule

mkProofTree :: RP.Proof -> Either String ProofTree
mkProofTree pr = do
    g <- justRight ("Cannot parse goal for proof tree") goal
    hs <- justRight ("Cannot parse hypotheses for proof tree") hyps
    en <- entr
    return ProofTree {
        proofName = name,
        proofConfidence = conf,
        proofIdents = ids,
        proofGoal = g,
        proofHypotheses = hs,
        proofManual = man,
        proofEntry = en
    }
    where reg = extractRegistry pr
          name = parseProofName $ RP.proofName pr
          conf = RP.proofConfidence pr
          ids  = [] -- TODO handle that
          goal = getPred reg $ RP.proofGoal pr
          hyps = sequence $ map (getPred reg) $ RP.proofHypotheses pr
          man  = RP.proofManual pr
          entr = mkRule reg goal $ RP.proofEntry pr


