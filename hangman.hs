module Hangman where

import Data.Char
import Data.List

hangman :: IO ()
hangman = do
  putStrLn "Enter your secret word: "
  word <- getWord
  putStrLn $ replicate 255 '\n'
  play word (confuscate (length word)) 5

getWord :: IO String
getWord = do
  word <- ((map toLower) . head . words) <$> getLine
  if not $ all (isAlpha) word
    then do
    putStrLn "Invalid word, please enter again: "
    getWord
    else
    return word

type Life = Int

confuscate :: Int -> String
confuscate n = replicate n '-'
unconfuscate :: [Int] -> Char -> String -> String
unconfuscate [] char confuscated = confuscated
unconfuscate (x:xs) char confuscated = take x confuscated ++ [char] ++
                                       unconfuscate (map (\y -> y - x - 1 ) xs) char (drop (x+1) confuscated)

play :: String -> String -> Life -> IO ()
play _ _ 0 = putStrLn "Unfortunately, you haven't managed to guess the magic word"
play password confuscated life = do
  putStrLn confuscated
  putStrLn $ show life ++ "/" ++ show 5 ++ " HP left"
  putStrLn "Try to guess a character: "
  char <- guess
  result <- process char password confuscated
  case result of
    Nothing -> do
      putStrLn "You've lost 1 HP!"
      play password confuscated (life - 1)
    Just occurences -> do
      let revealed = unconfuscate occurences char confuscated
      if revealed == password then do
        putStrLn $ "The answer is " ++ password
        putStrLn "You guesed the password and won the game!"
        else play password revealed life

indices :: (Eq a) => a -> [a] -> [Int]
indices y xs = [i | (x, i) <- zip xs [0,1..], x == y]

guess :: IO Char
guess = do
  char <- toLower <$> getChar
  if not $ isAlpha char
    then do
    putStrLn "Invalid character, please try again: "
    guess
    else
    return char

process :: Char -> String -> String -> IO (Maybe [Int])
process char password confuscated = let occurences = indices char password in
                                      if null occurences then do
                                        putStrLn "The following char doesn't occur"
                                        return Nothing
                                      else if char `elem` confuscated then do
                                        putStrLn "You already guessed this character"
                                        return Nothing
                                        else
                                        return (Just occurences)
