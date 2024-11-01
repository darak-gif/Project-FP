module Game where

import Colors (colors)
import WorkWithDictionary (getRandomWord)

type Suggestion = String
type SecretWord = String
type Answer = String 
type Len = Int

getSuggestion :: Len -> IO Suggestion
getSuggestion len = do 
    putStrLn "Enter your current suggestion!"
    putStrLn "If you want to end the game enter \"Quit\""
    line <- getLine
    if (line == "Quit" || line == "quit") 
        then do 
            putStrLn "You lost the game!"
            return line
    else 
        if length line == len 
            then do 
                putStrLn "Thank you!" 
                return line
        else do 
            putStrLn "The length of your suggestion is not right! Try again to enter a new suggestion!"
            getSuggestion len 


continueGame :: SecretWord -> IO Answer
continueGame secretWord = do
    suggestion <- getSuggestion $ length secretWord
    if suggestion == "Quit" || suggestion == "quit" then return suggestion
    else 
        if suggestion == secretWord 
            then do 
                putStrLn "You win! Congratulations!"
                return secretWord
        else do  
            putStrLn "Wrong suggestion! Try again!" 
            putStrLn "Your suggestion conform to the following array!"
            print $ colors secretWord suggestion
            continueGame secretWord

startGame :: Len -> IO Answer
startGame len = do 
    secretWord <- getRandomWord len
    continueGame secretWord
    


    