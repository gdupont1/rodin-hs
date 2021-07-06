------------------------------------------------------------------------
-- File: Rodin/Theory.hs - Part of rodinapi
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
Module      : Rodin.Theory
Description : Description of a theory in Rodin (as a syntactical tree)
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

This describes a Rodin theory, defining the Theory type.
Other submodules are responsible for other features.
-}
module Rodin.Theory where

import Rodin.Formula (Formula)

-- | Operator notation type
data NotationType = Prefix | Infix

-- | Operator properties: intermediate representation used in the operator definition
data OperatorProp =
    OperatorProp {
        associative  :: Bool,                                   -- ^ Is the operator associative
        commutative  :: Bool,                                   -- ^ Is the operator commutative
        formulaType  :: Bool,                                   -- ^ Is the operator an expression or a predicate
        notationType :: NotationType                            -- ^ Operator's notation type
    }
-- | An operator argument
data OperatorArgument =
    OperatorArgument {
        expression :: Formula,                                  -- ^ Operator argument type
        identifier :: String,                                   -- ^ Operator argument identifier
        opaComment :: Maybe String                              -- ^ Operator argument comment
    }
-- | An operator well-definedness condition
data OperatorWDCondition =
    OperatorWDCondition {
        predicate :: Formula,                                   -- ^ Well-definedness predicate
        wdComment :: Maybe String                               -- ^ Well-definedness comment
    }
-- | Operator's direct definition
data OperatorDirectDefinition =
    OperatorDirectDefinition {
        formula :: Formula,                                     -- ^ Direct definition formula
        ddComment :: Maybe String                               -- ^ Direct definition comment
    }
-- | Operator's recursive definition's case
data RecursiveDefinitionCase =
    RecursiveDefinitionCase {
        caseExpression :: Formula,                              -- ^ Case's expression (what it matches against)
        caseFormula    :: Formula,                              -- ^ Case's formula (its definition)
        caseComment    :: Maybe String                          -- ^ Case's comment
    }
-- | Operator's recursive definition
data OperatorRecursiveDefinition =
    OperatorRecursiveDefinition {
        inductiveArgument :: String,                            -- ^ Recursive definition's inductive argument (what should be matched)
        cases             :: [RecursiveDefinitionCase],         -- ^ List of cases
        recComment        :: Maybe String                       -- ^ Recursive definition's comment
    }

-- | Theory import
data ImportTheory =
    ImportTheory {
        theory :: String                                        -- ^ Import target
    }
-- | Theory project import
data ImportTheoryProject =
    ImportTheoryProject {
        project  :: String,                                     -- ^ Import target (project name)
        theories :: [ImportTheory]                              -- ^ Theories to be imported from that project
    }

-- | Theory's type parameter
data TypeParameter =
    TypeParameter {
        typeIdentifier :: String,                               -- ^ Type's identifier
        typeComment :: Maybe String                             -- ^ Type parameter's comment
    }

-- | An argument of a data-type constructor
data ConstructorArgument =
    ConstructorArgument {
        caId   :: String,                                       -- ^ Argument's identifier
        caType :: Formula,                                      -- ^ Argument's type
        caComment :: Maybe String                               -- ^ Argument's comment
    }
-- | A constructor for the data type
data DataTypeConstructor =
    DataTypeConstructor {
        dtcId :: String,                                        -- ^ Constructor's identifier
        args :: [ConstructorArgument],                          -- ^ Constructor's arguments
        dtcComment :: Maybe String                              -- ^ Constructor's comment
    }
-- | Type argument of a data-type
data TypeArgument =
    TypeArgument {
        typeArg :: Formula                                      -- ^ Type formula
    }
-- | Definition of a new data-type (composite type, enumerat    ion, recursive type...)
data DataTypeDefinition =
    DataTypeDefinition {
        dtId          :: String,                                -- ^ Data type identifier
        typeArguments :: [TypeArgument],                        -- ^ Data type type arguments
        constructors  :: [DataTypeConstructor],                 -- ^ Data type constructors
        dtComment     :: Maybe String                           -- ^ Data type comment
    }

-- | Definition of a (non-axiomatic) operator with its defin    ition
data NewOperatorDefinition =
    NewOperatorDefinition {
        opLabel  :: String,                                     -- ^ Operator name
        opProp   :: OperatorProp,                               -- ^ Operator properties
        opArgs   :: [OperatorArgument],                         -- ^ Operator arguments
        opWD     :: [OperatorWDCondition],                      -- ^ Operator (additionnal) well-definedness pre-conditions
        opDirDef :: [OperatorDirectDefinition],                 -- ^ Operator direct definition
        opRecDef :: [OperatorRecursiveDefinition],              -- ^ Operator recursive definition
        opComment :: Maybe String                               -- ^ Operator comment
    }

-- | Definition of an axiomatic type
data AxiomaticTypeDefinition =
    AxiomaticTypeDefinition {
        aTypeId :: String,                                      -- ^ New type name
        aTypeComment :: Maybe String                            -- ^ New type comment
    }
-- | Definition of an axiomatic operator (i.e. without direc    t definition)
data AxiomaticOperatorDefinition =
    AxiomaticOperatorDefinition {
        aOpLabel :: String,                                     -- ^ Operator name
        aOpProp  :: OperatorProp,                               -- ^ Operator properties
        aType    :: Formula,                                    -- ^ Operator type
        aOpArgs  :: [OperatorArgument],                         -- ^ Operator arguments
        aOpWD    :: [OperatorWDCondition],                      -- ^ Operator (additionnal) well-definedness pre-conditions
        aOpComment :: Maybe String                              -- ^ Operator comment
    }
-- | An axiomatic definition (i.e. an unproved assumption)
data AxiomaticDefinitionAxiom =
    AxiomaticDefinitionAxiom {
        aDefLabel     :: String,                                -- ^ Axiom label
        aDefPredicate :: Formula,                               -- ^ Axiom content as a predicate
        aDefComment   :: Maybe String                           -- ^ Axiom comment
    }
-- | A block of axiomatic definitions (a section containing     types, operators and axioms)
data AxiomaticDefinitionsBlock =
    AxiomaticDefinitionsBlock {
        aDefBLabel :: String,                                   -- ^ Block name
        aDefBTypes :: [AxiomaticTypeDefinition],                -- ^ Block axiomatic types
        aDefBDef   :: [AxiomaticOperatorDefinition],            -- ^ Block axiomatic operators
        aDefBAx    :: [AxiomaticDefinitionAxiom],               -- ^ Block axioms
        aDefBComment :: Maybe String                            -- ^ Block comment
    }

-- | A theorem
data Theorem =
    Theorem {
        thName      :: String,                                  -- ^ Theorem name/label
        thPredicate :: Formula,                                 -- ^ Theorem content, as a predicate
        thComment   :: Maybe String                             -- ^ Theorem comment
    }

-- | A meta-variable (i.e. substituable parameter) of a set     of rewrite/inference rules
data MetaVariable =
    MetaVariable {
        mvId   :: String,                                       -- ^ Variable name for later referencement
        mvType :: Formula,                                      -- ^ Variable type
        mvComment :: Maybe String                               -- ^ Variable comment
    }
-- | Given part (i.e. left hand side or antecedent) of an in    ference rule
data InferenceGiven =
    InferenceGiven {
        givenPredicate :: Formula,                              -- ^ Given part as a predicate
        givenHyp       :: Bool,                                 -- ^ If the given part must not be in an hypothesis (for auto-application purposes)
        givenComment   :: Maybe String                          -- ^ Given part comment
    }
-- | Result of an inference rule (i.e. right hand side)
data InferenceInfer =
    InferenceInfer {
        inferPredicate :: Formula,                              -- ^ Result as a predicate
        inferComment :: Maybe String                            -- ^ Result comment
    }
-- | An inference rule
data InferenceRule =
    InferenceRule {
        infLabel :: String,                                     -- ^ Inference rule label/name
        infApp   :: String,                                     -- ^ Rule applicability (automatic, manual or both)
        infDesc  :: String,                                     -- ^ Inference rule description
        given    :: [InferenceGiven],                           -- ^ Rule left hand side
        infer    :: [InferenceInfer],                           -- ^ Rule right hand side
        infComment :: Maybe String                              -- ^ Rule comment
    }
-- | Right hand side of a rewrite rule
data RewriteRuleRHS =
    RewriteRuleRHS {
        rhsLabel     :: String,                                 -- ^ RHS label (when their are multipl)
        rhsPredicate :: Formula,                                -- ^ Pre-condition or "filter" for determining when this RHS is to be preferred
        rhsFormula   :: Formula,                                -- ^ RHS "result"
        rhsComment   :: Maybe String                            -- ^ RHS comment
    }
-- | A rewrite rule
data RewriteRule =
    RewriteRule {
        rewLabel :: String,                                     -- ^ Rule label/name
        rewApp   :: String,                                     -- ^ Rule applicability (automatic, manual or both)
        complete :: Bool,                                       -- ^ If the rule is supposed to be exhaustive
        rewDesc  :: String,                                     -- ^ Rule description
        lhs      :: Formula,                                    -- ^ Rule left hand side (antecedent)
        rhs      :: [RewriteRuleRHS],                           -- ^ Rule right hand sides (possible results)
        rewComment :: Maybe String                              -- ^ Rule comment
    }
-- | A block of proof rules
data ProofRulesBlock =
    ProofRulesBlock {
        prBLabel       :: String,                               -- ^ Block label/name
        metaVariables  :: [MetaVariable],                       -- ^ Meta-variables for the rules of the block
        inferenceRules :: [InferenceRule],                      -- ^ Inference rules
        rewriteRules   :: [RewriteRule],                        -- ^ Rewrite rules
        prComment      :: Maybe String                          -- ^ Proof rule block comment
    }

-- | A theory
data Theory =
    Theory {
        theoryName           :: String,                         -- ^ Name of the theory
        imports              :: [ImportTheoryProject],          -- ^ Imported theories
        typeParameters       :: [TypeParameter],                -- ^ Theory type parameters (or "generecity parameters")
        datatypeDefinitions  :: [DataTypeDefinition],           -- ^ Defined data-types
        operators            :: [NewOperatorDefinition],        -- ^ Defined (non-axiomatic) operators
        axiomaticDefinitions :: [AxiomaticDefinitionsBlock],    -- ^ Axiomatic definitions (blocks)
        theorems             :: [Theorem],                      -- ^ Defined theorems
        proofRules           :: [ProofRulesBlock],              -- ^ Proof rules blocks
        theoryComment        :: Maybe String                    -- ^ Proof block comment
    }




