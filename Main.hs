module Main where

import Beginning
import Game (startGame)
import Easy (startEasyGame)
import Expert (startExpertGame)
import Helper (startHelperGame)

main :: IO ()
main = do 
    putStrLn "Welcome to the world of WORDLE"
    gameMode <- chooseGameMode  -- Use chooseGameMode to get the game mode
    len <- chooseLen -- Use chooseLen to get the length of the Secret word
    if gameMode == Helper 
        then do 
            startHelperGame len
            putStrLn "You played Helper Mode!"
    else if gameMode == (Game 0) 
        then do 
            startEasyGame len
            putStrLn "You played Easy Game!"
    else if gameMode == (Game 1)
        then do 
            startExpertGame len
            putStrLn "You played Expert Game!"
    else do 
        startGame len
        putStrLn "You played Standard Game!"
    putStrLn "Thank you for playing my game!"
    

        
                      

