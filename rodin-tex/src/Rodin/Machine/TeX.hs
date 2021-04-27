--------------------------------------------------------------------------
-- File: Rodin/Machine/TeX.hs - Part of rodinapi-tex
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
Module      : Rodin.Machine.TeX
Description : Module for printing a Machine to LaTeX
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This module mainly contains an instanciation of the 'Rodin.TeX.ShowTeX' for
the 'Rodin.Machine' type.
-}
module Rodin.Machine.TeX where

import Rodin.Internal.Util
import Rodin.TeX
import Rodin.Machine
import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.Formula.TeX

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.RefinesMachine' type
instance ShowTeX RefinesMachine where
  showTeX rm =
      "\n" ++ ind 1 ++ r
      where r = rmTarget rm

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.SeesContext' type
instance ShowTeX SeesContext where
  showTeX sc =
      "\n" ++ ind 1 ++ c
      where c = scTarget sc

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Variable' type
instance ShowTeX Variable where
  showTeX va =
      math $ printTeX' v
      where v = vaIdentifier va

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Invariant' type
instance ShowTeX Invariant where
  showTeX inv =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines'' 2 else math . printTeX') pr
      where lb = invLabel inv
            pr = invPred  inv

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Variant' type
instance ShowTeX Variant where
  showTeX vex =
      tail $ printTeXLines'' 1 ex
      where ex = varExpression vex

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.RefinesEvent' type
instance ShowTeX RefinesEvent where
  showTeX re =
      italic $ escape_ ev
      where ev = reTarget re

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Parameter' type
instance ShowTeX Parameter where
  showTeX par =
      math $ printTeX' $ pa
      where pa = paIdentifier par

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Guard' type
instance ShowTeX Guard where
  showTeX gu =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines'' 3 else math . printTeX') pr
      where lb = guLabel gu
            pr = guPred  gu

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Witness' type
instance ShowTeX Witness where
  showTeX wi =
      "\n" ++ ind 2 ++ (math $ printTeX' lb) ++ ": " ++
          (if any isNewline pr then printTeXLines'' 3 else math . printTeX') pr
      where lb = wiLabel wi
            pr = wiPred  wi

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Action' type
instance ShowTeX Action where
  showTeX ac =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printTeXLines'' 3 else math . printTeX') pr
      where lb = acLabel      ac
            pr = acAssignment ac

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.ConvergenceType' type
instance ShowTeX ConvergenceType where
  showTeX Ordinary = ""
  showTeX Convergent = " <convergent>"
  showTeX Anticipated = " <anticipated>"

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Event' type
instance ShowTeX Event where
  showTeX ev =
      "\n" ++ ind 1 ++ (bold $ escape_ lb) ++ showTeX co 
      ++ (if not $ null re then "\n" ++ ind 1 ++ "REFINES " ++ (texlist ","  re) else "")
      ++ (if not $ null pa then "\n" ++ ind 1 ++ "ANY  "    ++ (texlist ", " pa) else "")
      ++ (if not $ null gu then "\n" ++ ind 1 ++ "WHERE"    ++ (texlist ""   gu) else "") 
      ++ (if not $ null wi then "\n" ++ ind 1 ++ "WITH"     ++ (texlist ""   wi) else "") 
      ++ (if not $ null ac then "\n" ++ ind 1 ++ "THEN"     ++ (texlist ""   ac) else "") 
      ++ "\n" ++ ind 1 ++ "END\n"
      where lb = evLabel       ev
            co = evConvergence ev
            re = evRefines     ev
            pa = evParameters  ev
            gu = evGuards      ev
            wi = evWitnesses   ev
            ac = evActions     ev

-- | 'Rodin.TeX.ShowTeX' instance for the 'Rodin.Machine.Machine' type
instance ShowTeX Machine where
  showTeX ma =
      "MACHINE\n" ++ ind 1 ++ na 
      ++ (if not $ null re  then "\n" ++ "REFINES"    ++ (texlist ""   re ) else "") 
      ++ (if not $ null se  then "\n" ++ "SEES"       ++ (texlist ""   se ) else "") 
      ++ (if not $ null va  then "\n" ++ "VARIABLES  "++ (texlist ", " va ) else "")
      ++ (if not $ null inv then "\n" ++ "INVARIANTS" ++ (texlist ""   inv) else "") 
      ++ (if not $ null var then "\n" ++ "VARIANT"    ++ (texlist ""   var) else "") 
      ++ (if not $ null ev  then "\n" ++ "EVENTS"     ++ (texlist ""   ev ) else "") 
      ++ "\nEND"
      where na  = maName       ma
            re  = maRefines    ma
            se  = maSeesCtx    ma
            va  = maVariables  ma
            inv = maInvariants ma
            var = maVariants   ma
            ev  = maEvents     ma



