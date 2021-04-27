------------------------------------------------------------------------
-- File: Rodin/Theory/Read.hs - Part of rodinapi
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
Module      : Rodin.Theory.Read
Description : Rodin Theory XML file reader
Copyright   : Copyright (C) 2019 G. Dupont
License     : GPL-3
Maintainer  : guillaume.dupont@irit.fr

Module for reading a Rodin theories (.tuf) as an XML File.
-}
module Rodin.Theory.Read (
        parseTheoryFile
    ) where

import Rodin.Formula (Formula)
import Rodin.Formula.Tokenizer (tkn)
import Rodin.Theory
import Rodin.Internal.XML
import Rodin.Internal.Util (getRodinFileName)
import Data.Either.Combinators (fromRight)
import Data.List
import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)

-- | Parse a @notationType@ attribute into a 'NotationType'
parseNotationType :: String -> NotationType
parseNotationType "INFIX" = Infix
parseNotationType _ = Prefix

-- | Parse a set of attributes as the properties of an operator ('OperatorProp'). This set of
-- attributes is common to axiomatic and non-axiomatic operators.
parseOperatorProp :: Element -> OperatorProp
parseOperatorProp elt =
    OperatorProp {
        associative = (lkOrDef (pref TheoryCore "associative") attrskv "false") == "true",
        commutative = (lkOrDef (pref TheoryCore "commutative") attrskv "false") == "true",
        formulaType = (lkOrDef (pref TheoryCore "formulaType") attrskv "false") == "true",
        notationType = parseNotationType $ lkOrDef (pref TheoryCore "formulaType") attrskv "PREFIX"
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an operator argument
parseOperatorArgument :: Element -> OperatorArgument
parseOperatorArgument elt =
    OperatorArgument {
        expression = tkn $ lkOrDef (pref Core "expression") attrskv "",
        identifier =       lkOrDef (pref Core "identifier") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an operator well-definedness condition
parseOperatorWDCondition :: Element -> OperatorWDCondition
parseOperatorWDCondition elt =
    OperatorWDCondition {
        predicate = tkn $ lkOrDef (pref Core "predicate") (attrToTuple $ elAttribs elt) ""
    }

-- | Parse an operator direct definition
parseOperatorDirectDefinition :: Element -> OperatorDirectDefinition
parseOperatorDirectDefinition elt =
    OperatorDirectDefinition {
        formula = tkn $ lkOrDef (pref TheoryCore "formula") (attrToTuple $ elAttribs elt) ""
    }

-- | Parse an operator recursive definition case
parseRecursiveDefinitionCase :: Element -> RecursiveDefinitionCase
parseRecursiveDefinitionCase elt =
    RecursiveDefinitionCase {
        caseExpression = tkn $ lkOrDef (pref Core       "expression") attrskv "",
        caseFormula    = tkn $ lkOrDef (pref TheoryCore "formula")    attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an operator recursive definition
parseOperatorRecursiveDefinition :: Element -> OperatorRecursiveDefinition
parseOperatorRecursiveDefinition elt =
    OperatorRecursiveDefinition {
        inductiveArgument = lkOrDef (pref TheoryCore "inductiveArgument") (attrToTuple $ elAttribs elt) "",
        cases = parseChildren (isQName $ pref TheoryCore "recursiveDefinitionCase") parseRecursiveDefinitionCase elt
    }

-- | Parse a theory import
parseImportTheory :: Element -> ImportTheory
parseImportTheory elt =
    ImportTheory { theory = lkOrDef (pref TheoryCore "importTheory") (attrToTuple $ elAttribs elt) "" }

-- | Parse a theory project import
parseImportTheoryProject :: Element -> ImportTheoryProject
parseImportTheoryProject elt =
    ImportTheoryProject {
        project = lkOrDef (pref TheoryCore "importTheoryProject") (attrToTuple $ elAttribs elt) "",
        theories = parseChildren (isQName $ pref TheoryCore "importTheory") parseImportTheory elt
    }

-- | Parse a theory type parameter
parseTypeParameter :: Element -> TypeParameter
parseTypeParameter elt =
    TypeParameter { typeIdentifier = lkOrDef (pref Core "identifier") (attrToTuple $ elAttribs elt) "" }


-- | Parse a datatype constructor argument
parseConstructorArgument :: Element -> ConstructorArgument
parseConstructorArgument elt =
    ConstructorArgument {
        caId   =       lkOrDef (pref Core       "identifier") attrskv "",
        caType = tkn $ lkOrDef (pref TheoryCore "type")       attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a datatype constructor
parseDataTypeConstructor :: Element -> DataTypeConstructor
parseDataTypeConstructor elt =
    DataTypeConstructor {
        dtcId = lkOrDef (pref Core "identifier") attrskv "",
        args  = parseChildren (isQName $ pref TheoryCore "constructorArgument") parseConstructorArgument elt
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a datatype type argument
parseTypeArgument :: Element -> TypeArgument
parseTypeArgument elt =
    TypeArgument {
        typeArg = tkn $ lkOrDef (pref TheoryCore "givenType") (attrToTuple $ elAttribs elt) ""
    }

-- | Parse a datatype definition
parseDataTypeDefinition :: Element -> DataTypeDefinition
parseDataTypeDefinition elt =
    DataTypeDefinition {
        dtId = lkOrDef (pref Core "identifier") (attrToTuple $ elAttribs elt) "",
        typeArguments = parseChildren (isQName $ pref TheoryCore "typeArgument") parseTypeArgument elt,
        constructors  = parseChildren (isQName $ pref TheoryCore "datatypeConstructor") parseDataTypeConstructor elt
    }


-- | Parse the definition of a new (non-axiomatic) operator
parseNewOperatorDefinition :: Element -> NewOperatorDefinition
parseNewOperatorDefinition elt =
    NewOperatorDefinition {
        opLabel  = lkOrDef (pref Core "label") attrskv "",
        opProp   = parseOperatorProp elt,
        opArgs   = parseChildren (isQName $ pref TheoryCore "operatorArgument"            ) parseOperatorArgument            elt,
        opWD     = parseChildren (isQName $ pref TheoryCore "operatorWDcondition"         ) parseOperatorWDCondition         elt,
        opDirDef = parseChildren (isQName $ pref TheoryCore "directOperatorDefinition"    ) parseOperatorDirectDefinition    elt,
        opRecDef = parseChildren (isQName $ pref TheoryCore "recursiveOperationDefinition") parseOperatorRecursiveDefinition elt
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse the definition of an axiomatic type
parseAxiomaticTypeDefinition :: Element -> AxiomaticTypeDefinition
parseAxiomaticTypeDefinition elt =
    AxiomaticTypeDefinition { aTypeId = lkOrDef (pref Core "identifier") (attrToTuple $ elAttribs elt) "" }

-- | Parse the definition of an axiomatic operator
parseAxiomaticOperatorDefinition :: Element -> AxiomaticOperatorDefinition
parseAxiomaticOperatorDefinition elt =
    AxiomaticOperatorDefinition {
        aOpProp  = parseOperatorProp elt,
        aOpLabel = lkOrDef (pref Core "label") (attrToTuple $ elAttribs elt) "",
        aType    = tkn $ lkOrDef (pref TheoryCore "type") (attrToTuple $ elAttribs elt) "",
        aOpArgs  = parseChildren (isQName $ pref TheoryCore "operatorArgument"   ) parseOperatorArgument    elt,
        aOpWD    = parseChildren (isQName $ pref TheoryCore "operatorWDcondition") parseOperatorWDCondition elt
    }

-- | Parse the definition of an axiom
parseAxiomaticDefinitionAxiom :: Element -> AxiomaticDefinitionAxiom
parseAxiomaticDefinitionAxiom elt =
    AxiomaticDefinitionAxiom {
        aDefLabel     =       lkOrDef (pref Core "label"    ) attrskv "",
        aDefPredicate = tkn $ lkOrDef (pref Core "predicate") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse an axiomatic definition block
parseAxiomaticDefinitionsBlock :: Element -> AxiomaticDefinitionsBlock
parseAxiomaticDefinitionsBlock elt =
    AxiomaticDefinitionsBlock {
        aDefBLabel = lkOrDef (pref Core "label") (attrToTuple $ elAttribs elt) "",
        aDefBTypes = parseChildren (isQName $ pref TheoryCore "axiomaticTypeDefinition"    ) parseAxiomaticTypeDefinition     elt,
        aDefBDef   = parseChildren (isQName $ pref TheoryCore "axiomaticOperatorDefinition") parseAxiomaticOperatorDefinition elt,
        aDefBAx    = parseChildren (isQName $ pref TheoryCore "axiomaticDefinitionAxiom"   ) parseAxiomaticDefinitionAxiom    elt
    }

-- | Parse the definition of an operator
parseTheorem :: Element -> Theorem
parseTheorem elt =
    Theorem {
        thName      =       lkOrDef (pref Core "label"    ) attrskv "",
        thPredicate = tkn $ lkOrDef (pref Core "predicate") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a metavariable used in proof rules
parseMetaVariable :: Element -> MetaVariable
parseMetaVariable elt =
    MetaVariable {
        mvId   =       lkOrDef (pref Core       "identifier") attrskv "",
        mvType = tkn $ lkOrDef (pref TheoryCore "type"      ) attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse the given part of an inference rule
parseInferenceGiven :: Element -> InferenceGiven
parseInferenceGiven elt =
    InferenceGiven {
        givenPredicate = tkn $ lkOrDef (pref Core       "predicate") attrskv "",
        givenHyp       =      (lkOrDef (pref TheoryCore "hyp"      ) attrskv "") == "true"
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse the inferred part of an inference rule
parseInferenceInfer :: Element -> InferenceInfer
parseInferenceInfer elt = 
    InferenceInfer {
        inferPredicate = tkn $ lkOrDef (pref Core "predicate") (attrToTuple $ elAttribs elt) ""
    }

-- | Parse an inference rule
parseInferenceRule :: Element -> InferenceRule
parseInferenceRule elt =
    InferenceRule {
        infLabel = lkOrDef (pref Core   "label") attrskv "",
        infApp   = lkOrDef (pref TheoryCore "applicability") attrskv "",
        infDesc  = lkOrDef (pref TheoryCore "desc") attrskv "",
        given    = parseChildren (isQName $ pref TheoryCore "given") parseInferenceGiven elt,
        infer    = parseChildren (isQName $ pref TheoryCore "infer") parseInferenceInfer elt
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse the right-hand side of a rewrite rule
parseRewriteRuleRHS :: Element -> RewriteRuleRHS
parseRewriteRuleRHS elt =
    RewriteRuleRHS {
        rhsLabel     =       lkOrDef (pref Core       "label") attrskv "",
        rhsPredicate = tkn $ lkOrDef (pref Core       "predicate") attrskv "",
        rhsFormula   = tkn $ lkOrDef (pref TheoryCore "formula") attrskv ""
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a rewrite rule
parseRewriteRule :: Element -> RewriteRule
parseRewriteRule elt =
    RewriteRule {
        rewLabel =       lkOrDef  (pref Core       "label"        ) attrskv "",
        rewApp   =       lkOrDef  (pref TheoryCore "applicability") attrskv "",
        complete =      (lkOrDef  (pref TheoryCore "complete"     ) attrskv "") == "true",
        rewDesc  =       lkOrDef  (pref TheoryCore "desc"         ) attrskv "",
        lhs      = tkn $ lkOrDef  (pref TheoryCore "formula"      ) attrskv "",
        rhs      = parseChildren (isQName $ pref TheoryCore "rewriteRuleRHS") parseRewriteRuleRHS elt
    }
    where attrskv = attrToTuple $ elAttribs elt

-- | Parse a block of proof rules
parseProofRulesBlock :: Element -> ProofRulesBlock
parseProofRulesBlock elt =
    ProofRulesBlock {
        prBLabel       = lkOrDef (pref Core "label") (attrToTuple $ elAttribs elt) "",
        metaVariables  = parseChildren (isQName $ pref TheoryCore "metaVariable" ) parseMetaVariable  elt,
        inferenceRules = parseChildren (isQName $ pref TheoryCore "inferenceRule") parseInferenceRule elt,
        rewriteRules   = parseChildren (isQName $ pref TheoryCore "rewriteRule"  ) parseRewriteRule   elt
    }

-- | Parse a theory
parseTheory :: String -> Element -> Theory
parseTheory name elt =
    Theory {
        theoryName           = name,
        imports              = parseChildren (isQName $ pref TheoryCore "importTheoryProject"      ) parseImportTheoryProject elt,
        typeParameters       = parseChildren (isQName $ pref TheoryCore "typeParameter"            ) parseTypeParameter elt,
        datatypeDefinitions  = parseChildren (isQName $ pref TheoryCore "datatypeDefinition"       ) parseDataTypeDefinition elt,
        operators            = parseChildren (isQName $ pref TheoryCore "newOperatorDefinition"    ) parseNewOperatorDefinition elt,
        axiomaticDefinitions = parseChildren (isQName $ pref TheoryCore "axiomaticDefinitionsBlock") parseAxiomaticDefinitionsBlock elt,
        theorems             = parseChildren (isQName $ pref TheoryCore "theorem"                  ) parseTheorem elt,
        proofRules           = parseChildren (isQName $ pref TheoryCore "proofRulesBlock"          ) parseProofRulesBlock elt
    }

-- | Parse a theory file given as a @filename@ into a 'Rodin.Theory.Theory'
parseTheoryFile' :: String -> IO Theory
parseTheoryFile' filename =
    readFile filename >>= (return . parseXMLDoc) >>= (\r ->
        case r of
          Nothing -> putStrLn "Error while parsing document" >> (return $ Theory "" [] [] [] [] [] [] [])
          Just elt -> return $ parseTheory (getRodinFileName filename) elt)


parseTheoryFile :: String -> IO (Maybe Theory)
parseTheoryFile filename =
    readFile filename >>= (\str -> return $ (parseTheory (getRodinFileName filename)) <$> parseXMLDoc str)





