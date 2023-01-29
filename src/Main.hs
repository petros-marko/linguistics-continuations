module Main where

import Data.Char
import System.Environment
import Text.ParserCombinators.Parsec
import Continuation
import Language
import Translator
import Parser

--Define a model and test the functionality

--only Bjorn laughed
laughed :: E -> T
laughed = (==) Bjorn

--everyone smiled
smiled :: E -> T
smiled _ = True

--no one cried
cried :: E -> T
cried _ = False

--everyone likes themselves
--every person likes the next person in the list Bjorn | Benny | Frida | Noemi
--no other likes
likes :: E -> E -> T
likes x y
        |x == y = True
        |fromEnum x == (fromEnum y) + 1 = True
        |otherwise = False

--frida knows everyone
--everyone knows themselves
--no other relations
knows :: E -> E -> T
knows x y
        |x == Frida = True
        |x == y = True
        |otherwise = False

--define translation for "somebody"
--it's a function that expects a function as an argument,
--applies the function to every person and checks if any of the results are true
--because of currying we can ommit the argument that the function expects
somebody :: Continuation T E
somebody = Continuation $ (flip any) people

--similar definition for everybody
everybody :: Continuation T E
everybody = Continuation $ (flip all) people

--usual definition for or
or :: (E -> T) -> (E -> T) -> (E -> T)
or p q x = p x || q x

--usual definition for and
and :: (E -> T) -> (E -> T) -> (E -> T)
and p q x = p x && q x

--Synonym for Bool, to make notation more consistent
type T = Bool
--Entities in the universe
data E = Bjorn | Benny | Frida | Noemi deriving (Eq, Show, Enum)

people :: [E]
people = enumFrom $ toEnum 0

--a model using the above definitions
m :: Model E T
m = Model {
        --match the string version of everyone's name to themselves
        nameTranslations = [(map toLower (show e), e)| e <- people],
        quantifierNounPhraseTranslations = [("somebody", somebody), ("everybody", everybody), ("someone", somebody), ("everyone", everybody)],
        intransitiveVerbTranslations = [("laughed", laughed), ("smiled", smiled), ("cried", cried)],
        --notice we use flip likes and flip knows in the translation, which just switches the order
        --in which they expect their arguments. This is exactly what we did in class to accomodate
        --for word order in english
        transitiveVerbTranslations = [("likes",flip likes), ("knows", flip knows)],
        --Haskell has builting or and and functions that are slightly different
        --Main.or and Main.and ensures we use the functions above
        coordinateWordTranslations = [("or", Main.or), ("and", Main.and)]
}

--Take input sentences and evaluate them
main :: IO ()
main = do
        --extract command line arguments
    --flag: should the continuized grammar be used or the non-continuized (-c and -nc respectively) or we can just print the tree (-p)
    --raw: the string to parse
    flag:raw:_ <- getArgs
    --make every word lowercase to avoid parse errors
    let toParse = map toLower raw
    case flag of
        --if we are using the continuized grammar
        --parse the sentence
        "-c" -> case parse (sParser m) "" toParse of 
                -- if we succeeded translate the tree
                Right tree -> case translateS' m tree of
                            -- if that succeeds evaluate the remaining continuation and print the result
                            Just result -> putStrLn . show $ eval result
                            -- if the translation fails show the error message
                            -- this should never happen
                            Nothing     -> putStrLn $ "Some word used in the sentence was not defined in the model"
                -- if we get a parse error, print it
                Left err   -> putStrLn $ "Parse error: " ++ show err
        --if we are using the non continuized grammar
        --parse the sentenc3
        "-n" -> case parse (sParser m) "" toParse of 
                --if we succeeded in parsing, translate the tree
                Right tree -> case translateS m tree of
                            -- if the translation succeeds, print the reult
                            Just result -> putStrLn . show $ result
                            -- if the translation fails it is probably due to the use of qnps with a non-continuized grammar
                            Nothing     -> putStrLn $ "Some word in the sentence was not defined in the model. You cannot use quantifier noun phrases (e.g. Somebody) with a non-continuized grammar. Consider running the same command with the -c instead of the -n flag set"
                -- if we get a parse error, print it
                Left err   -> putStrLn $ "Parse error: " ++ show err

        --if we are just doing the parsing
        --parse the sentence
        "-p" -> case parse (sParser m) "" toParse of
                --if we succeeded in parsing, translate the tree
                Right tree -> putStrLn . show $ tree
                Left err   -> putStrLn $ "Parse error: " ++ show err
                -- if we get a parse error, print it
        -- one of -c, -n, -p needs to be used. Prompt the user appropriately
        _     -> putStrLn "Error! Correct use: continuations <-c|-n|-p> <string to parse>"
