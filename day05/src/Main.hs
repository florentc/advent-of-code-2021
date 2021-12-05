{-# LANGUAGE TupleSections #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

type Position = (Int, Int)

type Segment = (Position, Position)

isStraight :: Segment -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

isDiagonal :: Segment -> Bool
isDiagonal ((x1, y1), (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

points :: Segment -> [Position]
points s@((x1, y1), (x2, y2))
  | isStraight s || isDiagonal s =
    let (dx, dy) = (signum (x2 - x1), signum (y2 - y1))
     in zip [x1, x1 + dx .. x2] [y1, y1 + dy .. y2]
  | otherwise = []

intersections :: [Segment] -> Map Position Int
intersections = Map.unionsWith (+) . map (Map.fromList . map (,1) . points)

solve1 :: [Segment] -> Int
solve1 = Map.size . Map.filter (> 1) . intersections . filter isStraight

solve2 :: [Segment] -> Int
solve2 = Map.size . Map.filter (> 1) . intersections

type Parser = P.Parsec Void String

parserInput :: Parser [Segment]
parserInput = P.many (parserSegment <* P.newline)
  where
    parserSegment = (,) <$> parserPos <*> (P.string " -> " *> parserPos)
    parserPos = (,) <$> P.decimal <*> (P.char ',' *> P.decimal)

main :: IO ()
main = do
  input <- getContents
  case P.parse parserInput "stdin" input of
    Left bundle -> putStr (P.errorBundlePretty bundle)
    Right segments -> print (solve1 segments, solve2 segments)
