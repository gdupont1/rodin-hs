{-|
Module      : Rodin.Substitution
Description : a module for handling substitutions.
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

This module is a wrapper of the substitution library, that can
be used for various Rodin object.

The only thing substituable in practice are @Rodin.Formula@; the
idea is to propose a common interface to visit data structures, based
on the @Substituable@ typeclass.
-}
module Rodin.Substitution ( 
        Rule, Rules(..),
        substituteAll,
        substituteFormula, fromFile,
        collisions, compile,
        Substituable(..)
    ) where

import qualified Substitution as S
import qualified Substitution.Rule as SR
import qualified Substitution.Rule.Parser as SRP
import qualified Substitution.Pattern as SP
import qualified Substitution.Pattern.Parser as SPP
import qualified Substitution.Template as ST
import qualified Substitution.Template.Parser as STP
import Rodin.Ascii
import Rodin.Formula
import Rodin.Formula.Ascii
import Rodin.Formula.Tokenizer
import Rodin.Substitution.Label
import Control.Monad (forM, foldM)

-- | Type synonym for substitution rule, with correct labels
-- (@Rodin.Substitution.Label@) and tokens (@Rodin.Formula.Token@)
type Rule  = SR.Rule Label Token

-- | Type synonym for a list of rules (or a "substitution table")
type Rules = [Rule]

-- | Substitute a formula using a list of substitution rules
substituteFormula :: Rules -> Formula -> Formula
substituteFormula = S.substitute

-- | The `Substituable` typeclass represent any object that may be
-- substituted using a list of rules.
--
-- This serves as a base for a kind of visitor pattern: if a type
-- is of the form `data T a b = T1 a b | T2 a b | ...`, then
-- `substitute` consists in calling `substitute` (or `substituteAll`)
-- on every fields of every constructor.
--
-- If a type is of the form `data T' a = T' Formula a`, then `substitute`
-- essentially consissts in calling `substituteFormula` (this is the
-- implementation given in @Rodin.Formula.Substitution@).
class Substituable a where
  -- | Substitute an element of the type using the given rules
  substitute :: Rules -> a -> a
  substitute _ = id

-- | Substitute a list of elements using the given rules.
substituteAll :: (Substituable a) => Rules -> [a] -> [a]
substituteAll table = map (substitute table)

-- | Find collisions between the rules.
collisions :: Rules -> [Rules]
collisions = SR.collisions

-- | Compile a set of rules; i.e. compute the automaton associated 
-- with the rule's pattern (left hand side)
compile :: Monad m => Rules -> m Rules
compile rls =
    forM rls SR.compile

-- | Parse each line to extract a rule, or possibly an error
parseLines :: [String] -> [SRP.ParserResult Rule]
parseLines =
    SRP.parseLines SRP.defaultConfig parsePattern parseTemplate
    where parsePattern = SPP.parse SPP.defaultConfig parseLabel
          parseTemplate = STP.parse STP.defaultConfig parseFormula 
          parseFormula str =
              case tokenize showAscii str of
                Left _ -> Nothing
                Right a -> Just a

-- | Parse a file to extract a list of rules.
-- This function prints error and warning messages on the standard
-- output, and returns the number of errors caught during the parsing.
fromFile :: String -> IO (Int,Rules)
fromFile fn = 
    readFile fn >>= (return . parseLines . lines) >>= (foldM process (0,[])) >>= tclean
    where process (n,acc) (Left (SRP.AtLine SRP.EmptyRule ln)) = do
              putStrLn $ "Ignoring line " ++ show ln
              return (n, acc)
          process (n,acc) (Left e) = do
              putStrLn $ "Error: " ++ SRP.errstring e
              return (n + 1, acc)
          process (n,acc) (Right r) =
              case SR.check r of
                Nothing -> return (n, r:acc)
                Just er -> do
                    putStrLn $ "Error: " ++ SR.rerrstring er
                    return (n + 1, acc)
          tclean (n, r) = return (n, reverse r)







