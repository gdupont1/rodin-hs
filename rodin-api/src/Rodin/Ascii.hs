------------------------------------------------------------------------
-- File: Rodin/Ascii.hs - Part of rodinapi
------------------------------------------------------------------------
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
------------------------------------------------------------------------
{-|
Module      : Rodin.Ascii
Description : Generic definition for writing ASCII
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module mainly defines the ShowAscii typeclass, which classifies a type as
being convertible to plain ASCII.
-}
module Rodin.Ascii where

import Rodin.Internal.Util

-- | Class for representing a type that can be represented in ASCII
class ShowAscii a where
  -- | The only function to be defined is showAscii (that effectively transforms a object into ASCII)
  showAscii :: a -> String

-- | Helper function that applies showAscii to the given list and join the result with the given separator
asciilist :: (ShowAscii a) => String -> [a] -> String
asciilist = printlist showAscii


