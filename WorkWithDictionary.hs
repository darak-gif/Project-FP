module WorkWithDictionary where

import System.IO
import System.Random

type SecretWord = String
type Words = [String]
type Dictionary = [String]
type Len = Int

getWordsOfLengthN :: Len -> FilePath -> IO Words 
getWordsOfLengthN n filePath = do
    contents <- readFile filePath
    let wordsList = filter (\word -> length word == n) (words contents) -- words content takes a string and divides it into a list of words, where words are distinguished from empty characters 
    return wordsList

takeN :: Int -> [a] -> a
takeN n xs
    | n < 0         = error "Negative index!"
    | n >= length xs = error "Index out of bounds!"
    | otherwise     = xs !! n


getRandomWord :: Len -> IO SecretWord
getRandomWord len =  do 
    words <- getWordsOfLengthN len "dictionary.txt"
    n <- randomRIO (0, length words - 1)
    return $ takeN n words

