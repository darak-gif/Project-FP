module Easy where

import WorkWithDictionary (getRandomWord,takeN,getWordsOfLengthN)
import Game (getSuggestion)
import Colors (colors)

type SecretWord = String
type Suggestion = String
type Answer = String
type Words = [String]
type Len = Int

type GrayLetters = String
type YellowLetters = String
type GreenLetters = [(Int,Char)]

data GameState = GameState {
    grayLetters :: GrayLetters,
    yellowLetters :: YellowLetters,
    greenLetters :: GreenLetters
    } deriving (Show)

removeDublicatesInList :: (Eq a) => [a] -> [a]
removeDublicatesInList [] = []
removeDublicatesInList (x:xs) = if elem x xs then removeDublicatesInList xs else x : removeDublicatesInList xs

isMemberOfDictionary :: Suggestion -> Words -> Bool
isMemberOfDictionary suggestion words = elem suggestion words

knownGrayLetters :: Suggestion -> GameState -> GrayLetters
knownGrayLetters suggestion (GameState gray _ _) = removeDublicatesInList (filter (\ c -> elem c gray) suggestion)

notUsedYellowLetters :: Suggestion -> GameState -> YellowLetters
notUsedYellowLetters suggestion (GameState _ yell _) = filter (\ c -> not (elem c suggestion) && (elem c yell)) yell

wrongGreenLetters :: Suggestion -> GameState -> GreenLetters
wrongGreenLetters suggestion (GameState _ _ green) = filter (\ (pos,c) -> c /= takeN (pos - 1)  suggestion) green 

printLetters :: GrayLetters -> GrayLetters
printLetters [] = []
printLetters (x:xs) = show x ++ " " ++ printLetters xs

printGreenLetters :: GreenLetters -> String
printGreenLetters [] = ""
printGreenLetters (x:xs) = "("  ++ show (fst x) ++ "," ++ show (snd x) ++ ")" ++ printGreenLetters xs

newGreenLetters :: SecretWord -> Suggestion -> GreenLetters
newGreenLetters [] [] = []
newGreenLetters secretWord suggestion = helper 1 secretWord suggestion
    where 
        helper :: Int -> SecretWord -> Suggestion -> GreenLetters
        helper _ [] [] = []
        helper n (x:xs) (y:ys)
            | x == y = (n,x) : helper (n + 1) xs ys
            | otherwise = helper (n + 1) xs ys

updateGameState :: SecretWord -> Suggestion -> GameState -> GameState
updateGameState secretWord suggestion (GameState gray yellow green) =
    let inTheWord = filter (\c -> elem c secretWord) suggestion
        newYellow = removeDublicatesInList $ yellow ++ inTheWord
        notInTheWord = filter (\c -> not (elem c secretWord)) suggestion
        newGray = removeDublicatesInList $ gray ++ notInTheWord 
        correctLetters = newGreenLetters secretWord suggestion
        newGreen = removeDublicatesInList $ green ++ correctLetters
    in GameState newGray newYellow newGreen

infoGrayLetters :: Suggestion -> GameState -> IO ()
infoGrayLetters suggestion (GameState gray yellow green) = 
    if not (null (knownGrayLetters suggestion (GameState gray yellow green))) 
        then do 
            putStrLn "In your suggestion there are letters that you already know that the secret word do not contain!"
            putStrLn $ printLetters (knownGrayLetters suggestion (GameState gray yellow green))
    else putStrLn "Good job! In your suggestion there are not gray letters that you already know!"

infoYellowLetters :: Suggestion -> GameState -> IO ()
infoYellowLetters suggestion (GameState gray yellow green) = 
    if not (null (notUsedYellowLetters suggestion (GameState gray yellow green)))
        then do
            putStrLn "In your suggestion missed letters that you already know that the word contain!"
            putStrLn $ printLetters (notUsedYellowLetters suggestion (GameState gray yellow green))
    else putStrLn "Good job! In your suggestion there are not missing yellow letters that you already know!"

infoGreenLetters :: Suggestion -> GameState -> IO ()
infoGreenLetters suggestion (GameState gray yellow green) = 
    if not (null (wrongGreenLetters suggestion (GameState gray yellow green)))
        then do
            putStrLn "In your suggestion there are positions/is position that do not match the already known green letters!"
            putStrLn $ printGreenLetters (wrongGreenLetters suggestion (GameState gray yellow green))
    else putStrLn "Good job! In your suggestion all green letters that you already know are in the correct position!"

bonusInformation :: SecretWord -> Suggestion -> GameState -> IO ()
bonusInformation secretWord suggestion (GameState gray yellow green) = do
    words <- getWordsOfLengthN (length secretWord) "dictionary.txt"
    if not (isMemberOfDictionary suggestion words)
        then do
            putStrLn "This word is not in the dictionary!"
            infoGrayLetters suggestion (GameState gray yellow green)
            infoYellowLetters suggestion (GameState gray yellow green)
            infoGreenLetters suggestion (GameState gray yellow green)
    else do
        infoGrayLetters suggestion (GameState gray yellow green)
        infoYellowLetters suggestion (GameState gray yellow green)
        infoGreenLetters suggestion (GameState gray yellow green)

continueGame :: SecretWord -> GameState ->IO Answer
continueGame secretWord (GameState gray yellow green) = do
    suggestion <- getSuggestion $ length secretWord
    if suggestion == "Quit" || suggestion == "quit"
        then do 
            return suggestion
    else
        if suggestion == secretWord
            then do 
                putStrLn "You win! Congratulations!"
                return secretWord
        else do 
            putStrLn "Wrong suggestion! Try again!"
            putStrLn "Your suggestion conform to the following array!"
            print $ colors secretWord suggestion
            bonusInformation secretWord suggestion (GameState gray yellow green)
            continueGame secretWord (updateGameState secretWord suggestion (GameState gray yellow green))

startEasyGame :: Len -> IO Answer
startEasyGame len = do 
    secretWord <- getRandomWord len
    continueGame secretWord (GameState [] [] [])
    
