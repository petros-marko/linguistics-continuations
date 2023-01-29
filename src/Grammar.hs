module Grammar(S(..), VP(..), NP(..), VT(..), CP(..)) where

--Define what the syntax tree can look like
--This is a very limited grammar meant to only test specific things but it is easily built upon

-- A full sentence
data S  = S NP VP deriving (Eq, Show)
-- A verb phrase
-- Verb phrases can be:
--     Intransitive verbs
--     Transitive verbs followed by an object
--     Compound verb phrases; i.e. VP1 and/or VP2
data VP = VPi String | VPt VT NP | VPc VP CP deriving (Eq, Show)
-- Noun phrases
data NP = NP String deriving (Eq, Show)
-- This refers to a transitive verb, it could be incorporated in the VPt type constructor as a string
data VT = VT String deriving (Eq, Show)
-- Coordinate phrase; the latter half of a compound verb phrase
data CP = CP String VP deriving (Eq, Show)
