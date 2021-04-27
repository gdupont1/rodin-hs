--------------------------------------------------------------------------
-- File: Rodin/Context/TeX.hs - Part of rodinapi-tex
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
Module      : Rodin.Context.TeX
Description : Module for printing a Context to LaTeX
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module mainly contains an instanciation of the 'Rodin.TeX.ShowTeX'
typeclass for the 'Rodin.Context' type.
-}
module Rodin.Context.TeX where

import Rodin.Internal.Util
import Rodin.TeX
import Rodin.Context
import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.Formula.TeX

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Context.ExtendsContext'
instance ShowTeX ExtendsContext where
  showTeX ec =
    "\n" ++ ind 1 ++ tg
    where tg = ecTarget ec

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Context.Axiom'
instance ShowTeX Axiom where
  showTeX ax =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines'' 2 else math . printTeX') pr
      where lb = axLabel ax
            pr = axPred  ax

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Context.Context'
instance ShowTeX Constant where
  showTeX cst =
      "\n" ++ ind 1 ++ nm 
      where nm = ctName cst

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Context.CarrierSet'
instance ShowTeX CarrierSet where
  showTeX cs =
      "\n" ++ ind 1 ++ nm
      where nm = csName cs

-- | 'Rodin.TeX.ShowTeX' instance for 'Rodin.Context.Context'
instance ShowTeX Context where
  showTeX ctx =
      "CONTEXT\n" ++ ind 1 ++ nm ++
      (if not $ null ec then "\n" ++ "EXTENDS"   ++ (texlist "" ec) else "") ++
      (if not $ null cs then "\n" ++ "SETS"      ++ (texlist "" cs) else "") ++
      (if not $ null ct then "\n" ++ "CONSTANTS" ++ (texlist "" ct) else "") ++
      (if not $ null ax then "\n" ++ "AXIOMS"    ++ (texlist "" ax) else "") ++
      "\nEND"
      where nm = ctxName      ctx
            ec = ctxExtends   ctx
            ax = ctxAxioms    ctx
            ct = ctxConstants ctx
            cs = ctxSets      ctx



