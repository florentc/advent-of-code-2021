module Main where

import Control.Arrow (first, (&&&))
import Data.Bits (xor)
import Data.Foldable (foldl')
import Data.List (partition)

parseInput :: String -> Maybe [[Bool]]
parseInput = mapM (mapM parseBit) . lines
  where
    parseBit '0' = Just False
    parseBit '1' = Just True
    parseBit _ = Nothing

frequencies :: [[Bool]] -> [Int]
frequencies = foldr (zipWith (\b n -> n + if b then 1 else -1)) (repeat 0)

powerConsumption :: [[Bool]] -> Integer
powerConsumption input =
  let bits = map (> 0) . frequencies $ input
   in bitsToDecimal bits * bitsToDecimal (not <$> bits)

bitsToDecimal :: [Bool] -> Integer
bitsToDecimal = foldl' (\n b -> n * 2 + if b then 1 else 0) 0

lifeSupportRating :: [[Bool]] -> Maybe Integer
lifeSupportRating numbers =
  let (x, y) = prepare numbers in (*) <$> aux True (x, y) <*> aux False (x, y)
  where
    prepare :: [[Bool]] -> ([([Bool], Integer)], [Int])
    prepare = fmap (id &&& bitsToDecimal) &&& frequencies
    aux :: Bool -> ([([Bool], Integer)], [Int]) -> Maybe Integer
    aux _ ([(_, n)], _) = Just n
    aux mode (entries, f : fs) =
      let (kept, removed) =
            partition ((== (mode `xor` (f >= 0))) . head . fst) entries
          (kept', removed') = (first tail <$> kept, tail . fst <$> removed)
       in aux mode (kept', updateFrequencies fs removed')
    aux _ _ = Nothing
    updateFrequencies :: [Int] -> [[Bool]] -> [Int]
    updateFrequencies = foldr (zipWith (\b n -> n + if b then -1 else 1))

main :: IO ()
main = do
  mInput <- parseInput <$> getContents
  case mInput of
    Nothing -> putStrLn "Could not parse input"
    Just input -> (print . (powerConsumption &&& lifeSupportRating)) input
