module Rodin.ProofTree.Printer where

import Rodin.Ascii
import Rodin.Formula
import Rodin.Formula.Ascii
import Rodin.ProofTree
import Rodin.ProofTree.Folder
import Rodin.Proofs (PrIdent, pridName, pridType)
import Data.List

_indent :: Int -> String
_indent n = take n $ repeat ' '

indentate :: Int -> [String] -> [String]
indentate n =
    map (i++)
    where i = _indent n

indentate1 :: String -> [String] -> [String]
indentate1 _ [] = []
indentate1 p (x:xs) = (p ++ x):(indentate (length p) xs)

section :: Int -> String -> [String] -> [String]
section _ _ [] = []
section pad s (x:xs) =
    (secn ++ x):(map (padi++) xs)
    where compaux n acc
            | n >= pad = acc
            | otherwise = compaux (n + 1) (acc ++ [' '])
          secn = compaux (length s) s
          padi = _indent pad

printRefPred :: RefPred -> String
printRefPred rp = 
    "(" ++ name ++ ") " ++ showAscii pred
    where name = predicate_name rp
          pred = predicate rp

printRefPreds :: [RefPred] -> String
printRefPreds =
    (intercalate ", ") . printRefPredsLines

printRefPredsLines :: [RefPred] -> [String]
printRefPredsLines = map printRefPred

printRefPredsSection :: Int -> String -> [RefPred] -> [String]
printRefPredsSection pad s = (section pad s) . (map printRefPred)

printIdent :: PrIdent -> String
printIdent i =
    name ++ " : " ++ showAscii typp
    where name = pridName i
          typp = pridType i

printIdents :: [PrIdent] -> String
printIdents is =
    intercalate ", " $ map printIdent is

printHA :: HypAction -> [String]
printHA (SELECT _ l) = printRefPredsSection 9 "SELECT" l
printHA (DESELECT _ l) = printRefPredsSection 9 "DESELECT" l
printHA (HIDE _ l) = printRefPredsSection 9 "HIDE" l
printHA (SHOW _ l) = printRefPredsSection 9 "SHOW" l
printHA (FORWARD_INF _ lt li) =
    from ++ inf
    where from = printRefPredsSection 9 "FROM" lt
          inf  = printRefPredsSection 9 "INFER" li
printHA (REWRITE _ lt li lh) =
    from ++ inf ++ hid
    where from = printRefPredsSection 9 "REWRITE" lt
          inf  = printRefPredsSection 9 "TO" li
          hid  = printRefPredsSection 9 "HIDING" lh

printAnte :: Antecedent -> [String]
printAnte ante =
    [goal] ++ (indentate 1 (addh ++ addi ++ rule))
    where has  = concat $ map printHA $ hypActions ante
          addh = printRefPredsSection 9 "ADDING" $ addedHypotheses ante
          addi =
            case addedIdentifiers ante of
              [] -> []
              l -> ["ADDING   " ++ (printIdents l)]
          goal = "(" ++ (antecedentName ante) ++ ") |- " ++ (printRefPred $ antecedentGoal ante)
          rule = indentate1 "=> " $ printRule $ antecedentRule ante

printRule :: Rule -> [String]
printRule rule = 
    [name] ++ hyps ++ [goal] ++ ants
    where name = "(" ++ ruleName rule ++ ") " ++ display rule ++ " [" ++ ruleRID rule ++ "]"
          goal = " |- " ++ (printRefPred $ ruleGoal rule)
          hyps = printRefPredsSection 5 "HYPS" $ hypotheses rule
          ants = concat $ map ((indentate1 " - ") . printAnte) $ antecedents rule

printProofName :: ProofName -> String
printProofName pn =
    let i = 
            case proofLabel pn of
              Nothing -> ""
              Just l -> '/':i
    in (proofSource pn) ++ i ++ "/" ++ (proofType pn)

printProofTree :: ProofTree -> [String]
printProofTree pt =
    [name] ++ hyps ++ [goal] ++ ent
    where name = (printProofName $ proofName pt) ++ ":"
          hyps = printRefPredsSection 5 "HYPS" $ proofHypotheses pt
          goal = "PROVE " ++ (printRefPred $ proofGoal pt)
          ent  = indentate1 " - " $ printRule $ proofEntry pt



