module Main where

import Data.Foldable (foldl')

parseInput :: String -> Maybe [[Bool]]
parseInput = mapM (mapM parseBit) . lines
  where
    parseBit '0' = Just False
    parseBit '1' = Just True
    parseBit _ = Nothing

deltaBit :: Bool -> Int -> Int
deltaBit b n = n + if b then 1 else -1

mostFrequentBits :: [[Bool]] -> [Bool]
mostFrequentBits = map (> 0) . foldr (zipWith deltaBit) (repeat 0)

bitsToDecimal :: [Bool] -> Integer
bitsToDecimal = foldl' (\n b -> n * 2 + if b then 1 else 0) 0

solve1 :: [[Bool]] -> Integer
solve1 input =
  let bits = mostFrequentBits input
   in bitsToDecimal bits * bitsToDecimal (not <$> bits)

main :: IO ()
main = do
  mInput <- parseInput <$> getContents
  case mInput of
    Nothing -> putStrLn "Could not parse input"
    Just input -> print $ solve1 input
