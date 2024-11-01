module Colors where

import WorkWithDictionary (takeN)

type SecretWord = String
type Suggestion = String
type Colors = [String]

-- Game mode

colors :: SecretWord -> Suggestion -> Colors
colors secretWord suggestion = helper 0 secretWord suggestion
  where 
    helper :: Int -> SecretWord -> Suggestion -> Colors
    helper _ _ [] = []
    helper pos secretWord (y:ys) 
      | (takeN pos secretWord) == y = "Green" : helper (pos + 1) secretWord ys 
      | elem y secretWord = "Yellow" : helper (pos + 1) secretWord ys
      | otherwise = "Gray" : helper (pos + 1) secretWord ys

-- Helper mode 

isCorrectEnteredColors :: Colors -> Bool
isCorrectEnteredColors colors =  (all (\ color -> (color == "Green" || color == "green") 
    || (color == "Gray" || color == "gray") || (color == "Yellow" || color == "yellow") ) colors ) 

getColors :: IO Colors
getColors = do 
    putStrLn "Enter the colors that matched to my last suggestion!"
    line <- getLine
    let colors = (words line)
    if colors == ["Quit"] || colors == ["quit"] then return ["Quit"]
    else    
        if (isCorrectEnteredColors colors) then return colors
        else do 
            putStrLn "Your input is not in the correct format! Try again!"
            getColors