------------------------------------------------------------------------
-- File: Rodin/Context/Ascii.hs - Part of rodinapi
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
Module      : Rodin.Context.Ascii
Description : Ascii converter for Event-B contexts
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.com

Instances of the 'Rodin.Ascii.ShowAscii' typeclass for the 'Rodin.Context.Context' type.
-}
module Rodin.Context.Ascii where

import Rodin.Internal.Util
import Rodin.Ascii
import Rodin.Context
import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.Formula.Ascii

-- | 'ShowAscii' instance for 'Rodin.Context.ExtendsContext'
instance ShowAscii ExtendsContext where
  showAscii ec =
    "\n" ++ ind 1 ++ tg
    where tg = ecTarget ec

-- | 'ShowAscii' instance for 'Rodin.Context.Axiom'
instance ShowAscii Axiom where
  showAscii ax =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 2 else printAscii) pr
      where lb = axLabel ax
            pr = axPred  ax

-- | 'ShowAscii' instance for 'Rodin.Context.Theorem'
instance ShowAscii Theorem where
  showAscii th =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 2 else printAscii) pr
      where lb = thLabel th
            pr = thPred  th

-- | 'ShowAscii' instance for 'Rodin.Context.Constant'
instance ShowAscii Constant where
  showAscii cst =
      "\n" ++ ind 1 ++ nm 
      where nm = ctName cst

-- | 'ShowAscii' instance for 'Rodin.Context.CarrierSet'
instance ShowAscii CarrierSet where
  showAscii cs =
      "\n" ++ ind 1 ++ nm
      where nm = csName cs

-- | 'ShowAscii' instance for 'Rodin.Context.Context'
instance ShowAscii Context where
  showAscii cx =
      "CONTEXT\n" ++ ind 1 ++ nm ++
      (if not $ null ec then "\n" ++ "EXTENDS"   ++ (asciilist "" ec) else "") ++
      (if not $ null cs then "\n" ++ "SETS"      ++ (asciilist "" cs) else "") ++
      (if not $ null ct then "\n" ++ "CONSTANTS" ++ (asciilist "" ct) else "") ++
      (if not $ null ax then "\n" ++ "AXIOMS"    ++ (asciilist "" ax) else "") ++
      (if not $ null th then "\n" ++ "THEOREMS"  ++ (asciilist "" th) else "") ++
      "\nEND"
      where nm = ctxName      cx
            ec = ctxExtends   cx
            ax = ctxAxioms    cx
            th = ctxTheorems  cx
            ct = ctxConstants cx
            cs = ctxSets      cx



