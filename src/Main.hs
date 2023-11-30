{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Control.Lens
import Data.List
import Data.Maybe
import Data.Text (Text)
import Monomer
import Monomer.Lens qualified as L
import System.IO
import System.Random
import TextShow

-- Types definition
type MainWord = String

type Guess = String

data GuessResult = GuessResult
  { correctPlace :: [Char],
    wrongPlace :: [Char],
    incorrect :: [Char]
  }
  deriving (Show)

data GameState = GameState {targetWord :: MainWord, guesses :: [Guess], maxAttempts :: Int} deriving (Show)

wordBankFilePath :: FilePath
wordBankFilePath = "Words/wordle-answers-alphabetical.txt"

readWordBank :: FilePath -> IO [MainWord]
readWordBank filePath = do
  contents <- readFile filePath
  return $ lines contents

-- Function to check a guess against the target word and produce a GuessResult
checkGuess :: MainWord -> Guess -> GuessResult
checkGuess target guess =
  let correct = [ch | (ch, idx) <- zip guess [0 ..], target !! idx == ch]
      remainingTarget = filter (`notElem` correct) target
      remainingGuess = filter (`notElem` correct) guess
      correctButWrongPlace = intersect remainingTarget remainingGuess
      incorrectGuess = remainingGuess \\ correctButWrongPlace
   in GuessResult correct correctButWrongPlace incorrectGuess

-- Function to update the game state based on the guess and the current state
updateGameState :: Guess -> GameState -> GameState
updateGameState guess state@(GameState target guessesLeft attempts) =
  let result = checkGuess target guess
      newGuesses = guess : guessesLeft
      newAttempts = attempts - 1
   in GameState target newGuesses newAttempts

-- Function to play the game
playWordle :: IO ()
playWordle = do
  wordBankFromFile <- readWordBank wordBankFilePath
  target <- pickRandomWord wordBankFromFile
  let initialState = GameState target [] 6
  playRound initialState

pickRandomWord :: [MainWord] -> IO MainWord
pickRandomWord words = do
  index <- randomRIO (0, length words - 1)
  return (words !! index)

playRound :: GameState -> IO ()
playRound state@(GameState target _ attempts)
  | attempts == 0 = putStrLn "You've run out of attempts. Game over!"
  | otherwise = do
      putStrLn $ "Attempts left: " ++ show attempts
      putStrLn "Enter your guess (5-letter word): "
      guess <- getLine
      if length guess /= 5 || not (all (`elem` ['a' .. 'z']) guess)
        then putStrLn "Invalid input. Please enter a 5-letter word."
        else do
          let newState = updateGameState guess state
              result = checkGuess target guess
          putStrLn $ "Correctly placed: " ++ correctPlace result
          putStrLn $ "Correct but wrong place: " ++ wrongPlace result
          putStrLn $ "Incorrect: " ++ incorrect result
          if length (correctPlace result) == 5
            then putStrLn "Congratulations! You guessed the word!"
            else playRound newState

-- Main function the game
main :: IO ()
main = playWordle
