module Expert where

import System.Random
import WorkWithDictionary (getRandomWord,takeN)
import Game (getSuggestion)
import Colors (colors)
import Easy (removeDublicatesInList)

type SecretWord = String
type Suggestion = String -- Yes, I know these synonyms are already defined in the Game.hs, but I want to see the type while reading Expert.hs
type Answer = String

type EnteredLetters = String -- Already Entered letters (words) from Player 
type NotEnteredLetters = String
type WrongColors = [String]
type ToBeGray = String
type ToBeYellowOrGreen = String

type Len = Int
type Try = Int

getRandomTry :: IO Try
getRandomTry = randomRIO (1,3)

lettersThatAreNotInEnteredFromSuggestion :: Suggestion -> EnteredLetters -> NotEnteredLetters
lettersThatAreNotInEnteredFromSuggestion suggestion entered = removeDublicatesInList $ filter (\ c -> not (elem c entered)) suggestion

lettersThatAreInSecretWordFromSuggestion :: SecretWord -> NotEnteredLetters -> ToBeGray
lettersThatAreInSecretWordFromSuggestion secretWord notEntered = removeDublicatesInList $ filter (\ c -> elem c secretWord) notEntered

lettersThatAreNotInSecretWordFromSuggestion :: SecretWord -> NotEnteredLetters -> ToBeYellowOrGreen
lettersThatAreNotInSecretWordFromSuggestion secretWord notEntered = removeDublicatesInList $ filter (\ c -> not (elem c secretWord)) notEntered
-- Yes is the same as lettersThatAreNotInEnteredFromSuggestion but for better readability of the code

cheater :: SecretWord -> Suggestion -> EnteredLetters -> WrongColors
cheater secretWord suggestion entered = 
    let 
        tobeGray = lettersThatAreInSecretWordFromSuggestion secretWord (lettersThatAreNotInEnteredFromSuggestion suggestion entered)
        toBeYellowOrGreen = lettersThatAreNotInSecretWordFromSuggestion secretWord (lettersThatAreNotInEnteredFromSuggestion suggestion entered)
    in falseColors secretWord suggestion  tobeGray toBeYellowOrGreen

chooseYellowOrGreen :: Int ->  String -- I will use the position that I have anyway so that I do not use randomRIO again
chooseYellowOrGreen n 
  | mod n 2 == 0 = "Yellow"
  | otherwise = "Green"

falseColors :: SecretWord -> Suggestion -> ToBeGray -> ToBeYellowOrGreen -> WrongColors
falseColors secretWord suggestion gray yellow = helper 0 secretWord suggestion 
    where 
        helper :: Int -> SecretWord -> Suggestion -> WrongColors
        helper _ _ [] = []
        helper pos secretWord (y:ys) 
            | elem y gray = "Gray" : helper (pos + 1) secretWord ys
            | elem y yellow  = (chooseYellowOrGreen pos) : helper (pos + 1) secretWord ys
            | elem y secretWord = "Yellow" : helper (pos + 1) secretWord ys
            | (takeN pos secretWord) == y = "Green" : helper (pos + 1) secretWord ys 
            | otherwise = "Gray" : helper (pos + 1) secretWord ys 

updateEnteredLetters :: Suggestion -> EnteredLetters -> EnteredLetters
updateEnteredLetters suggestion entered = removeDublicatesInList (suggestion ++ entered)

continueGame :: SecretWord -> Try -> Try -> EnteredLetters -> IO Answer 
continueGame secretWord try currTry entered = do
    suggestion <- getSuggestion $ length secretWord
    if suggestion == "Quit" || suggestion == "quit"
        then do
            return suggestion
    else 
        if suggestion == secretWord 
           then do 
               putStrLn "You win! Congratulations!"
               return secretWord
        else  
            if (try /= currTry) 
                then do
                    putStrLn "Wrong suggestion! Try again!" 
                    putStrLn "Your suggestion conform to the following array!"
                    print $ colors secretWord suggestion
                    continueGame secretWord try (currTry + 1) (updateEnteredLetters suggestion entered)
            else do
                putStrLn "Wrong suggestion! Try again!" 
                putStrLn "Your suggestion conform to the following array!"
                print $ cheater secretWord suggestion entered 
                continueGame secretWord try (currTry + 1) (updateEnteredLetters suggestion entered)



startExpertGame :: Len -> IO Answer
startExpertGame len = do
    secretWord <- getRandomWord len
    try <- getRandomTry
    continueGame secretWord try 1 [] 


