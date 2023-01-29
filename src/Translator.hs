module Translator(translateS,translateS') where

-- Define how to translate Syntax trees into truth values using the typed lambda calculus
-- we take advantage of haskell's built in support for higher order curried functions and
-- thus do not define the typed lambda calculus from scratch
-- First we define how to translate normally and then give alternative translations for 
-- what the paper refers to as "continuized" grammar
-- All the translations return values wrapped in Maybe, since we cannot translate a sentence
-- if a translation is not given in the model

import Control.Applicative
import Continuation
import Grammar
import Language

-- utility function to find the translation of a word in a list if it exists
-- this could be implemented better with a map, but it is kept for readability
findInPairs :: (Eq a) => a -> [(a,b)] -> Maybe b
findInPairs _ [] = Nothing
findInPairs x ((f,s):ys) = if x == f then Just s else findInPairs x ys

--Translations of non continuized grammar

--to translate an NP just look for the word in the names list
--we ignore the quantifier noun phrases since we do not support them in the
--non-continuized grammar
translateNP :: Model e t -> NP -> Maybe e
translateNP m (NP n) = findInPairs n $ nameTranslations m

--to translate a verb phrase we go case by case
translateVP :: Model e t -> VP -> Maybe (e -> t)
--intransitive verbs are the easiest, and we just need to look in the list
translateVP m (VPi v) = findInPairs v $ intransitiveVerbTranslations m
--transitive verbs are more interesting. we use the applicative operaiton <*>
--to apply the function that the verb represents to the argument that is the object
--through the maybe wrapper
translateVP m (VPt v o) = (translateVT m v) <*> (translateNP m o)
--similarly for the compound verb we translate the parts separately and appropriately
--apply the coordinate word function to the verb argument through the maybe wrapper
translateVP m (VPc v c) = (translateCP m c) <*> (translateVP m v) 

--translating vt is similarly just a lookup in the appropriate list
translateVT :: Model e t -> VT -> Maybe (e -> e -> t)
translateVT m (VT v) = findInPairs v $ transitiveVerbTranslations m

--to translate the coordinate phrase we just look for the coord word in the word list and then
--apply the function to the verb argument
translateCP :: Model e t -> CP -> Maybe ((e -> t) -> (e -> t))
translateCP m (CP c v) = (findInPairs c $ coordinateWordTranslations m) <*> (translateVP m v)

--finally a sentence is just a noun phrase followed by a verb phrase, and the translation of the noun is applied to
--the translation of the verb
translateS :: Model e t -> S -> Maybe t
translateS m (S n v) = (translateVP m v) <*> (translateNP m n)

--Translations in the continuized grammar

--translating a noun phrase now requires us to look into the quantifier noun phrase word list
--if we have a quantifier noun phrase we just return it since it is already a continuaiton
--otherwise we wrap the regular name in a continuation and return that
translateNP' :: Model e t -> NP -> Maybe (Continuation t e)
translateNP' m (NP n) = findInPairs n qnps <|> (return <$> findInPairs n nps)
    where qnps = quantifierNounPhraseTranslations m
          nps  = nameTranslations m

--verb phrases are similarly done case by case
--instead of the regular function application operation used above
--in the continuized grammar version we combine the sub-expressions using the
--lifted function application operation
translateVP' :: Model e t -> VP -> Maybe (Continuation t (e -> t))
translateVP' m (VPt v o) = ($$) <$> (translateVT' m v) <*> (translateNP' m o)
translateVP' m (VPc v c) = ($$) <$> (translateCP' m c) <*> (translateVP' m v)
translateVP' m vp = (return <$>) . (translateVP m) $ vp

--similarly we need to wrap things into a continuation before returning them
translateVT' :: Model e t -> VT -> Maybe (Continuation t (e -> e -> t))
translateVT' m  = (return <$>) . (translateVT m)

--similarly we need to wrap things into a continuation before returning them
translateCP' :: Model e t -> CP -> Maybe (Continuation t ((e -> t) -> (e -> t)))
translateCP' m (CP c v) = (($$) . return) <$> (findInPairs c $ coordinateWordTranslations m) <*> (translateVP' m v)

--notice how the sentence translation is identical to the original one save for the fact that we are using the
--lifted function application operation
translateS' :: Model e t -> S -> Maybe (Continuation t t)
translateS' m (S n v) = ($$) <$> (translateVP' m v) <*> (translateNP' m n)
