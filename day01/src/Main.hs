module Main where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import Safe.Exact (takeExactMay)
import Text.Read (readMaybe)

parseInput :: String -> Maybe [Integer]
parseInput = mapM readMaybe . lines

nbIncreases :: Ord a => [a] -> Int
nbIncreases [] = 0
nbIncreases [_] = 0
nbIncreases (x1 : x2 : xs) = (if x1 < x2 then 1 else 0) + nbIncreases (x2 : xs)

windows :: Int -> [a] -> [[a]]
windows size = mapMaybe (takeExactMay size) . tails

nbIncreasesWindows :: (Num a, Ord a) => [a] -> Int
nbIncreasesWindows = nbIncreases . map sum . windows 3

main :: IO ()
main =
  do
    mInput <- parseInput <$> getContents
    case mInput of
      Nothing -> putStrLn "Could not parse input"
      Just input -> do
        putStrLn "Number of increases"
        print $ nbIncreases input
        putStrLn "Number of increases (sliding window of 3)"
        print $ nbIncreasesWindows input
