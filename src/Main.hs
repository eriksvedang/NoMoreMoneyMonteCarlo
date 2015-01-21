module Main where

import Control.Monad
import Control.Monad.State
import System.Random
import Data.List

data GameInfo = GameInfo {
  gameRound   :: Integer, -- how many rounds of the game has been played
  gameCrashes :: Integer, -- total number of crashes
  gameDanger  :: Integer  -- current danger level
} deriving Show

mkGame = GameInfo {
  gameRound = 0,
  gameCrashes = 0,
  gameDanger = 1
}

main = do
  let runs = 1000
  putStrLn "NO MORE MONEY"
  printExperiment runs 4
  printExperiment runs 5

printExperiment :: Int -> Integer -> IO ()
printExperiment runs crashLimit = do
  g <- newStdGen
  let gameLengths = experiment g runs crashLimit
  let average = (fromIntegral (sum gameLengths)) / (fromIntegral runs)
  let freqs = (sort (frequencies gameLengths))
  putStrLn "-------------------------------------------------"
  putStrLn ("Using " ++ show crashLimit ++ " crashes, game will average " ++ show average ++ " rounds.\n")
  putStrLn "Rounds \t\tFrequencies\tPercentage"
  mapM_ (printFreq runs) freqs

printFreq :: Int -> (Integer, Int) -> IO ()
printFreq runs (rounds, freq) =
  putStrLn $ show rounds ++ "\t\t" ++
             show freq ++ "\t\t" ++
             show (100 * (fromIntegral freq) / (fromIntegral runs)) ++ "%"

experiment :: StdGen -> Int -> Integer -> [Integer]
experiment g runs crashLimit = map gameRound games
                               where games = evalState (replicateM runs (simulateGame crashLimit mkGame)) g

simulateGame :: Integer -> GameInfo -> State StdGen GameInfo
simulateGame crashLimit gameInfo = do
  let gameRound' = 1 + gameRound gameInfo
  crashesThisTurn <- newCrashes (gameDanger gameInfo)
  let gameCrashes' = crashesThisTurn + gameCrashes gameInfo
  let gameInfo' = gameInfo {
        gameRound = gameRound',
        gameCrashes = gameCrashes'
      }
  if gameCrashes' >= crashLimit
    then return gameInfo'
    else simulateGame crashLimit gameInfo'
  
newCrashes :: Integer -> State StdGen Integer
newCrashes danger = do
  a <- rollDie 4
  b <- rollDie 6
  c <- rollDie 12
  d <- rollDie 20
  return ((crashVal danger a) + (crashVal danger b) + (crashVal danger c) + (crashVal danger d))

crashVal dangerLevel dieRoll = if dieRoll <= dangerLevel then 1 else 0

rollDie :: Integer -> State StdGen Integer
rollDie sides = do
  g <- get
  let (x, g') = randomR (1, sides) g
  put g'
  return x

frequencies :: Eq a => [a] -> [(a, Int)]
frequencies xs = [(c, length $ filter (== c) xs) | c <- nub xs]

