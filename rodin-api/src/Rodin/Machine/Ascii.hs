------------------------------------------------------------------------
-- File: Rodin/Machine/Ascii.hs - Part of rodinapi
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
Module      : Rodin.Machine.Ascii
Description : Ascii transformation of Rodin machines
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

Instance of 'Rodin.Ascii.ShowAscii' for 'Rodin.Machine'.
-}
module Rodin.Machine.Ascii where

import Rodin.Internal.Util
import Rodin.Ascii
import Rodin.Machine
import Rodin.Formula
import Rodin.Formula.Internal.Util
import Rodin.Formula.Ascii

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.RefinesMachine'
instance ShowAscii RefinesMachine where
  showAscii rm =
    "\n" ++ ind 1 ++ r
    where r = rmTarget rm

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.SeesContext'
instance ShowAscii SeesContext where
  showAscii sc =
    "\n" ++ ind 1 ++ c
    where c = scTarget sc

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Variable'
instance ShowAscii Variable where
  showAscii va =
    "\n" ++ ind 1 ++ printAscii v
    where v = vaIdentifier va

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Invariant'
instance ShowAscii Invariant where
  showAscii inv =
      "\n" ++ ind 1 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 2 else printAscii) pr
      where lb = invLabel inv
            pr = invPred  inv

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Variant'
instance ShowAscii Variant where
  showAscii var =
      tail $ printAsciiLines 1 ex
      where ex = varExpression var


-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.RefinesEvent'
instance ShowAscii RefinesEvent where
  showAscii = reTarget

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Parameter'
instance ShowAscii Parameter where
  showAscii par =
      "\n" ++ ind 2 ++ printAscii pa
      where pa = paIdentifier par

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Guard'
instance ShowAscii Guard where
  showAscii gu =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 3 else printAscii) pr
      where lb = guLabel gu
            pr = guPred  gu

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Witness'
instance ShowAscii Witness where
  showAscii wi =
      "\n" ++ ind 2 ++ printAscii lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 3 else printAscii) pr
      where lb = wiLabel wi
            pr = wiPred  wi

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Action'
instance ShowAscii Action where
  showAscii ac =
      "\n" ++ ind 2 ++ lb ++ ": " ++
          (if any isNewline pr then printAsciiLines 3 else printAscii) pr
      where lb = acLabel      ac
            pr = acAssignment ac

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.ConvergenceType'
instance ShowAscii ConvergenceType where
  showAscii Ordinary    = ""
  showAscii Convergent  = " convergent"
  showAscii Anticipated = " anticipated"

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Event'
instance ShowAscii Event where
  showAscii ev =
      "\n" ++ ind 1 ++ lb ++ showAscii co ++
      (if not $ null re then "\n" ++ ind 1 ++ "REFINES " ++ (asciilist "," re) else "") ++
      (if not $ null pa then "\n" ++ ind 1 ++ "ANY"      ++ (asciilist ""  pa) else "") ++
      (if not $ null gu then "\n" ++ ind 1 ++ "WHERE"    ++ (asciilist ""  gu) else "") ++
      (if not $ null wi then "\n" ++ ind 1 ++ "WITH"     ++ (asciilist ""  wi) else "") ++
      (if not $ null ac then "\n" ++ ind 1 ++ "THEN"     ++ (asciilist ""  ac) else "") ++
      "\n" ++ ind 1 ++ "END\n"
      where lb = evLabel       ev
            co = evConvergence ev
            re = evRefines     ev
            pa = evParameters  ev
            gu = evGuards      ev
            wi = evWitnesses   ev
            ac = evActions     ev

-- | 'Rodin.Ascii.ShowAscii' instance for 'Rodin.Machine.Machine'
instance ShowAscii Machine where
  showAscii ma =
      "MACHINE\n" ++ ind 1 ++ na ++
      (if not $ null re  then "\n" ++ "REFINES"    ++ (asciilist "" re ) else "") ++
      (if not $ null se  then "\n" ++ "SEES"       ++ (asciilist "" se ) else "") ++
      (if not $ null va  then "\n" ++ "VARIABLES"  ++ (asciilist "" va ) else "") ++
      (if not $ null inv then "\n" ++ "INVARIANTS" ++ (asciilist "" inv) else "") ++
      (if not $ null var then "\n" ++ "VARIANT"    ++ (asciilist "" var) else "") ++
      (if not $ null ev  then "\n" ++ "EVENTS"     ++ (asciilist "" ev ) else "") ++
      "\nEND"
      where na  = maName       ma
            re  = maRefines    ma
            se  = maSeesCtx    ma
            va  = maVariables  ma
            inv = maInvariants ma
            var = maVariants   ma
            ev  = maEvents     ma



