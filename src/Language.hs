module Language(Model(..), names, quantifierNounPhrases, intransitiveVerbs, transitiveVerbs, coordinateWords) where

-- Define what a model is and some useful utility functions for parsing

import Continuation

-- A model has dual purpose:
--     It defines which words are allowed in sentences
--     It defines how those words are translated into formal expressions
-- This is done by giving lists of pairs where a word is matched with its translation
-- Model takes two type parameters e and t, which are the entity and truth types respectively
-- names are objects of type e
-- quantifierNounPhrases are objects of type (e -> t) -> t, and our main point of interest
-- quantifierNounPhrases have type Continuation t e actually, to signify that they should only
-- exist in the continuized grammar
-- they are distinct because they are treated differently in the translation step
data Model e t = Model {
    nameTranslations :: [(String, e)],
    quantifierNounPhraseTranslations :: [(String, Continuation t e)],
    intransitiveVerbTranslations :: [(String, (e -> t))],
    transitiveVerbTranslations :: [(String,(e -> e -> t))],
    coordinateWordTranslations :: [(String, ((e -> t) -> (e -> t) -> (e -> t)))]
}

--Utility functions to extract the allowed words in each category

names :: Model e t -> [String]
names = (map fst) . nameTranslations

quantifierNounPhrases :: Model e t -> [String]
quantifierNounPhrases  = (map fst) . quantifierNounPhraseTranslations

intransitiveVerbs :: Model e t -> [String]
intransitiveVerbs = (map fst) . intransitiveVerbTranslations

transitiveVerbs :: Model e t -> [String]
transitiveVerbs = (map fst) . transitiveVerbTranslations

coordinateWords :: Model e t -> [String]
coordinateWords = (map fst) . coordinateWordTranslations
