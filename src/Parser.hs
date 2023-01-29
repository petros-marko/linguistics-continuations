module Parser(sParser) where

--Define functions to parse a string into a syntax tree
--everything follows immediately from the definitions in Grammar.hs

import Text.ParserCombinators.Parsec
import Language
import Grammar

--Utilities
--parse one or more space characters
ws :: Parser String
ws = many1 (char ' ')
--parse one of the words in a given list
oneOfWords :: [String] -> Parser String
oneOfWords = choice . (map (try . Text.ParserCombinators.Parsec.string))
--parse a subexpression using f, a second subexpression using s and combine the results with c
twoArgParser :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
twoArgParser f s c = c <$> f <*> (ws >> s)

--a parser for a sentence; parses a noun phrase, then a verb phrase adn then combines them into a sentence
sParser :: Model e t -> Parser S
sParser m = twoArgParser (npParser m) (vpParser m) S

--a parser for a noun phrase
--parse one of the allowed noun-words (names and quantifier noun phrases) and wrap it in an NP
npParser :: Model e t -> Parser NP
npParser m = oneOfWords (names m ++ quantifierNounPhrases m) >>= return . NP

--a parser for a verb phrase
--it is defined as trying to parse the different kind of verb phrases and keeping the first succssful one
vpParser :: Model e t -> Parser VP
vpParser m = choice . (map try) $ choices
    --find one of the allowed intransitive verb words
    where vpiParser = oneOfWords (intransitiveVerbs m) >>= return . VPi
          --parse a transitive verb and a noun and combine them into a VPt
          vptParser = twoArgParser (vtParser m) (npParser m) VPt
          vpsParser = try vpiParser <|> try vptParser
          --parse a verb phrase, then a coordinate phrase and combine them into a VPc
          vpcParser = twoArgParser vpsParser (cpParser m) VPc
          choices = [vpcParser, vptParser, vpiParser]

--parse a transitive verb word
vtParser :: Model e t -> Parser VT
vtParser m = oneOfWords (transitiveVerbs m) >>= return . VT

--parse a coordinate phrase
--first parse a coordinate word then a verb phrase combine them into a CP
cpParser :: Model e t -> Parser CP
cpParser m = twoArgParser (oneOfWords $ coordinateWords m) (vpParser m) CP
