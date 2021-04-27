--------------------------------------------------------------------------
-- File: Rodin/TeX.hs - Part of rodinapi-tex
--------------------------------------------------------------------------
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
--------------------------------------------------------------------------
{-|
Module      : Rodin.TeX
Description : Module defining the typeclass and functions needed for exporting
              things to LaTeX
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module defines the ShowTeX typeclass for denoting types that can be
printed to (La)TeX. It also defines a few utilities.
-}
module Rodin.TeX where

import Rodin.Internal.Util
import Data.Char (isSpace)

-- | The @ShowTeX@ typeclass. A type implementing this typeclass can be
-- printed to LaTeX.
class ShowTeX a where
  -- | The only function to define is how to actually print the type in LaTeX
  showTeX :: a -> String

-- | Escape a string in a listing. The target file is expected to be used with
-- the @listings@ LaTeX package, which should be configured so that (* *) denote
-- an inline escaping.
escape :: String -> String
escape = circle "(*" "*)"

-- | Put the given string in LaTeX math mode (effectively surrounding it with
-- dollar signs).
-- The target file is expected to be used with the @listings@ LaTeX package, 
-- which should be configured so that $ is auto-escaping (@mathescape@ option).
math :: String -> String
math = circle "$" "$"

-- | Put the given string in LaTeX math mode, but put any leading space
-- before the dollar sign (so that it is actually printed).
mathspace :: String -> String
mathspace input =
    let (spaces,remaining) = span isSpace input in
        spaces ++ (math remaining)

-- | Put a string in (LaTeX) bold
bold :: String -> String
bold = escape . circle "\\textbf{" "}"

-- | Put a string in (LaTeX) italic
italic :: String -> String
italic = escape . circle "\\textit{" "}"

-- | Print a list of 'ShowTeX' instances using 'showTeX' and intercalating
-- the resulting @[String]@ with the given parameter.
texlist :: (ShowTeX a) => String -> [a] -> String
texlist = printlist showTeX

-- | Escape a string before converting it to LaTeX. This is especially used
-- on identifiers and special Event-B structures so that printing is flawless.
escape_ :: String -> String
escape_ =
    foldl (++) "" . map (\x -> if x `elem` needescape then ['\\',x] else [x])
    where needescape = "_\\{}[]^"



