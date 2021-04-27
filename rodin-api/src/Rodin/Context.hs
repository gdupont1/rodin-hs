------------------------------------------------------------------------
-- File: Rodin/Context.hs - Part of rodinapi
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
Module      : Rodin.Context
Description : Description of a context in Rodin (as a syntactical tree)
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This describes a Rodin context, defining the Context type.
Other submodules are responsible for other features.
-}
module Rodin.Context where

import Rodin.Formula (Formula)

-- | Extends Context relationship
data ExtendsContext = ExtendsContext {
    ecTarget :: String      -- ^ Which context is being extended
} 

-- | Axiom definition
data Axiom = Axiom {
    axLabel :: String,      -- ^ Axiom's label
    axPred :: Formula,      -- ^ Axiom's predicate
    axIsTheorem :: Bool     -- ^ Is the axiom a theorem
}

-- | Constant definition
data Constant = Constant {
    ctName :: String        -- ^ Constant's name
}

-- | Carrier set definition
data CarrierSet = CarrierSet {
    csName :: String        -- ^ Set's name
}

-- | Context definition
data Context = Context {
    ctxName      :: String,                 -- ^ Context name
    ctxExtends   :: [ExtendsContext],       -- ^ Context extension relationships
    ctxAxioms    :: [Axiom],                -- ^ Axioms
    ctxConstants :: [Constant],             -- ^ Constants
    ctxSets      :: [CarrierSet]            -- ^ Carrier sets
}


