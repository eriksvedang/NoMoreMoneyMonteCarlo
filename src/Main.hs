module Main where

import Control.Monad
import Control.Monad.State
import System.Random
import Data.List
import Numeric

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

data DangerIncreaseMode = Increasing | AlwaysAtOne deriving Eq

data GameSettings = GameSettings {
  settingsCrashLimit :: Integer,
  settingsDangerIncrease :: DangerIncreaseMode
}

main = do
  let runs = 1000
  putStrLn $ "NO MORE MONEY, " ++ show runs ++ " runs"
  printExperiment runs (GameSettings 4 AlwaysAtOne)
  printExperiment runs (GameSettings 5 AlwaysAtOne)
  printExperiment runs (GameSettings 4 Increasing)
  printExperiment runs (GameSettings 5 Increasing)

printExperiment :: Int -> GameSettings -> IO ()
printExperiment runs settings = do
  g <- newStdGen
  let gameLengths = runExperiment g runs settings
  let average = (fromIntegral (sum gameLengths)) / (fromIntegral runs)
  let freqs = (sort (frequencies gameLengths))
  putStrLn "\n-------------------------------------------------\n"
  putStrLn ("Using " ++ show (settingsCrashLimit settings) ++ " crashes" ++
           (if ((settingsDangerIncrease settings) == Increasing) then " and increasing danger level" else "") ++
           ", game will average " ++ show average ++ " rounds:\n")
  putStrLn "Rounds\tFreq\tFrac\tPercentage"
  mapM_ (printFreq runs) freqs

printFreq :: Int -> (Integer, Int) -> IO ()
printFreq runs (rounds, freq) = do
  let frac = (fromIntegral freq) / (fromIntegral runs)
  putStrLn $ show rounds ++ "\t" ++
             show freq ++ "\t" ++
             showFFloat (Just 3) frac "\t" ++ 
             showFFloat (Just 0) (frac * 100) "%"

runExperiment :: StdGen -> Int -> GameSettings -> [Integer]
runExperiment g runs settings = map gameRound games
                               where games = evalState (replicateM runs (simulateGame settings mkGame)) g

simulateGame :: GameSettings -> GameInfo -> State StdGen GameInfo
simulateGame settings gameInfo = do
  let crashLimit = settingsCrashLimit settings
  let gameRound' = 1 + gameRound gameInfo
  crashesThisTurn <- newCrashes (gameDanger gameInfo)
  let gameCrashes' = crashesThisTurn + gameCrashes gameInfo
  let shouldIncreaseDanger = (settingsDangerIncrease settings) == Increasing && crashesThisTurn == 0
  let gameDanger'  = if shouldIncreaseDanger then 1 + (gameDanger gameInfo) else 1
  let gameInfo' = gameInfo {
        gameRound   = gameRound',
        gameCrashes = gameCrashes',
        gameDanger  = gameDanger'
      }
  if gameCrashes' >= crashLimit
    then return gameInfo'
    else simulateGame settings gameInfo'
  
newCrashes :: Integer -> State StdGen Integer
newCrashes danger = do
  rolls <- mapM rollDie [4, 6, 12, 20]
  return $ sum (map (crashVal danger) rolls)

crashVal :: Integer -> Integer -> Integer
crashVal dangerLevel dieRoll = if dieRoll <= dangerLevel then 1 else 0

rollDie :: Integer -> State StdGen Integer
rollDie sides = do
  g <- get
  let (x, g') = randomR (1, sides) g
  put g'
  return x

frequencies :: Eq a => [a] -> [(a, Int)]
frequencies xs = [(c, length $ filter (== c) xs) | c <- nub xs]

