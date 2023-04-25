------------------------------------------------------------------------
-- File: Main.hs - Main file for proof-read
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
Module      : Main
Description : Main module for this project
Copyright   : (c) Guillaume Dupont, 2023
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

Main module, entry point.
-}
module Main where

import qualified Rodin.Proofs as RP
import Rodin.Proofs.Read
import Rodin.ProofTree
import Rodin.ProofTree.Arrange
import Rodin.ProofTree.Printer
import Control.Monad
import System.Environment

readPT :: String -> IO [ProofTree]
readPT filename =
    (parseProofFileFile filename) 
    >>= (\pf ->
        case pf of
          Nothing -> putStrLn ("Error when reading " ++ filename) >> return []
          Just x -> return $ RP.pfProofs x)
    >>= (\ps -> reverse <$> foldM read1 [] ps)
    where read1 acc p =
            case mkProofTree p of
              Left e -> putStrLn ("Ignoring proof " ++ (RP.proofName p) ++ ": " ++ e) >> return acc
              Right pt -> return (pt:acc)

printPT :: ProofTree -> IO ()
printPT = (mapM_ putStrLn) . printProofTree

underline :: String -> String
underline [] = []
underline (x:xs) = '=':(underline xs)

main :: IO ()
main = do
    files <- getArgs
    forM_ files $ \x -> do
        putStrLn $ x ++ ":"
        putStrLn $ underline (x ++ ":")
        pts <- readPT x
        forM_ pts $ \p -> do
            printPT p
            putStrLn ""
        putStrLn ""
    putStrLn ""
    putStrLn "Done."


