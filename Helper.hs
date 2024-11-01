module Helper where

import WorkWithDictionary (getWordsOfLengthN,takeN)
import Colors (isCorrectEnteredColors,getColors)
import Easy (removeDublicatesInList)

type Dictionary = [String]
type Suggestion = String
type Colors = [String]
type RemovedWords = [(Int,Int)] -- The key Int for position in current dictionary other for count of removed words in current dictionary

type GrayLetters = String
type YellowLetters = String -- Yes, I know these synonyms are already defined in the Easy.hs, but I want to see the type while reading Helper.hs
type GreenLetters = [(Int,Char)]

type Len = Int

data GameState = GameState {
    grayLetters :: GrayLetters,
    yellowLetters :: YellowLetters,
    greenLetters :: GreenLetters
    } deriving (Show)

newGrayLetters :: Suggestion -> Colors -> GrayLetters
newGrayLetters suggestion colors = helper suggestion colors
    where 
        helper :: Suggestion -> Colors -> GrayLetters 
        helper [] [] = []
        helper (x:xs) (y:ys) = if y == "Gray" || y == "gray" then x : helper xs ys else helper xs ys 

newYellowLetters :: Suggestion -> Colors -> YellowLetters
newYellowLetters suggestion colors = helper suggestion colors
    where 
        helper :: Suggestion -> Colors -> YellowLetters 
        helper [] [] = []
        helper (x:xs) (y:ys) = if y == "Yellow" || y == "yellow" then x : helper xs ys else helper xs ys

newGreenLetters :: Suggestion -> Colors -> GreenLetters
newGreenLetters suggestion colors = helper 1 suggestion colors
    where 
        helper :: Int -> Suggestion -> Colors -> GreenLetters
        helper _ [] [] = []
        helper pos (x:xs) (y:ys) = if y == "Green" || y == "green" then (pos,x) : helper (pos + 1) xs ys else helper (pos + 1) xs ys

updateGameState :: Suggestion -> Colors -> GameState -> GameState
updateGameState suggestion colors (GameState gray yellow green) = 
    let 
        newGray = removeDublicatesInList $ gray ++ (newGrayLetters suggestion colors)
        newYellow = removeDublicatesInList $ yellow ++ (newYellowLetters suggestion colors)
        newGreen = removeDublicatesInList $ green ++ (newGreenLetters suggestion colors)
    in GameState newGray newYellow newGreen

checkGreenLetters :: String -> GreenLetters -> Bool
checkGreenLetters word green = all (\ (pos,char) -> (takeN (pos - 1) word) == char) green

checkYellowLetters :: String -> YellowLetters -> Bool
checkYellowLetters word yellow = all (\ c -> elem c word) yellow

checkNotContainsGrayLetter :: String -> GrayLetters -> Bool
checkNotContainsGrayLetter word gray = all (\ c -> not (elem c word)) gray

newDictionary :: GameState -> Dictionary -> Dictionary
newDictionary (GameState gray yellow green) currDictionary = 
    (filter (\ word -> checkGreenLetters word green && checkNotContainsGrayLetter word gray && checkYellowLetters word yellow) currDictionary)
    
countRemovedWords :: Dictionary -> RemovedWords
countRemovedWords currDictionary = helper 0 currDictionary
    where 
        helper :: Int -> Dictionary -> RemovedWords
        helper _ [] = []
        helper pos (x:xs) = 
            let 
            count = foldl (+) 0 (map (\ word -> if or (map (\ c -> elem c word) x) then 1 else 0 ) currDictionary)
            in  (pos,count) : helper (pos + 1) xs

getSuggestion :: Dictionary -> RemovedWords -> Suggestion
getSuggestion [] _ = ""
getSuggestion currDictionary removedWords = 
    let 
        pair = (foldr (\ (p1,c1) (p2,c2) -> if c1 >= c2 then (p1,c1) else (p2,c2)) (head removedWords) (tail removedWords))
        pos = fst pair
    in  (takeN pos currDictionary)

continueGame :: Suggestion -> Dictionary -> RemovedWords -> GameState -> IO Colors
continueGame suggestion currDictionary removedWords (GameState gray yellow green) = do
    putStrLn "My current suggestion is:"
    putStrLn suggestion
    putStrLn "If you want to end the game enter \"Quit\""
    colors <- getColors
    if (colors == ["Quit"] || colors == ["quit"]) 
        then do 
            putStrLn "You lost the game!"
            return colors
    else if (length suggestion /= length colors) 
        then do 
            putStrLn "The length of your enterens is not right!Try again!"
            continueGame suggestion currDictionary removedWords (GameState gray yellow green)
    else 
        if all (\ color -> color == "Green" || color == "green") colors
            then do
                putStrLn "VICTORY! Game is over!"
                return colors
        else
            let 
                newGameState = updateGameState suggestion colors (GameState gray yellow green)
                newDic = newDictionary newGameState currDictionary
                newRemovedWords = countRemovedWords newDic 
            in
                if (null newDic)
                    then do 
                        putStrLn "Your enteres does not matched to any word in the dictionary!Try again!"
                        continueGame suggestion currDictionary removedWords (GameState gray yellow green)
                else do
                    continueGame (getSuggestion newDic newRemovedWords) newDic newRemovedWords newGameState

startHelperGame :: Len -> IO Colors
startHelperGame len = do 
    dictionary <- getWordsOfLengthN len "dictionary.txt"
    putStrLn "Choose one word!"
    print $ dictionary 
    putStrLn "Remember it!"
    continueGame (getSuggestion dictionary (countRemovedWords dictionary)) dictionary (countRemovedWords dictionary) (GameState [] [] [])