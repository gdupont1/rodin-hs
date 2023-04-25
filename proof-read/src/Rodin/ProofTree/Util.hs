module Rodin.ProofTree.Util where

import qualified Data.Map as M
import qualified Rodin.Proofs as RP
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Control.Monad (sequence)
import Rodin.Formula
import Rodin.ProofTree
import Rodin.ProofTree.Registry (Registry, getPred)

parseProofName :: String -> ProofName
parseProofName name =
    let (x1:x2:q) = splitOn "/" name in
        let x3 = concat q in
            if null x3
                then ProofName { proofSource = x1, proofType = x2, proofLabel = Nothing }
                else ProofName { proofSource = x1, proofType = x3, proofLabel = Just x2 }

parseHA :: Registry -> RP.PrHypAction -> Maybe HypAction
parseHA reg ha =
    let (name, nums) = span (not . isDigit) $ RP.haName ha in
        let num = if null nums then 0 else read nums in
            case name of
              "SELECT"      -> SELECT      num <$> (get RP.haHypotheses)
              "DESELECT"    -> DESELECT    num <$> (get RP.haHypotheses)
              "HIDE"        -> HIDE        num <$> (get RP.haHypotheses)
              "SHOW"        -> SHOW        num <$> (get RP.haHypotheses)
              "FORWARD_INF" -> FORWARD_INF num <$> (get RP.haHypotheses) <*> (get RP.haInfHypotheses)
              "REWRITE"     -> REWRITE     num <$> (get RP.haHypotheses) <*> (get RP.haInfHypotheses) <*> (get RP.haHidden)
              _ -> Nothing
    where get proj = sequence $ map (getPred reg) (proj ha)





