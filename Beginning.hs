module Beginning where


data GameMode = Game { difficulty :: Int }  -- 0 for easy game and 1 for expert ans 2 for standard game
              | Helper deriving (Eq, Show)
 

chooseGameMode :: IO GameMode
chooseGameMode = do
    putStrLn "Please choose the mode of a game: Enter \"Game\" or \"Helper\""
    mode <- getLine
    if mode == "Game" || mode == "game" then do
        putStrLn "Choose difficulty: Enter \"Easy\" or \"Expert\" or you can choose standard game with entering \"Standard\""
        difficulty <- getLine
        if difficulty == "Easy" || difficulty == "easy" then return (Game 0)
        else if difficulty == "Expert" || difficulty == "expert" then return (Game 1)
        else if difficulty == "Standard" || difficulty == "standard" then return (Game 2)
        else do
            putStrLn "Wrong difficulty! Try again!"
            chooseGameMode
    else if mode == "Helper" || mode == "helper" then return Helper
    else do
        putStrLn "Wrong mode! Try again!"
        chooseGameMode

getInt :: IO Int
getInt = do line <- getLine
            return $ read line

chooseLen :: IO Int
chooseLen = do 
    putStrLn "Please enter your desired length in the range [4,10]"
    n <- getInt 
    if n <= 10 && n >= 4 then return n 
    else do
            putStrLn "The length must be in the range [4,10]! Try again!"
            chooseLen
