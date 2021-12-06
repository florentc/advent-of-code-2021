module Main where

import Control.Arrow ((&&&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

agesMap :: [Int] -> Map Int Integer
agesMap = Map.unionsWith (+) . map (`Map.singleton` 1)

step :: Map Int Integer -> Map Int Integer
step ages =
  Map.unionsWith
    (+)
    [ Map.fromList [(age, numberAged (age + 1) ages) | age <- [0 .. 7]],
      Map.singleton 6 (numberAged 0 ages),
      Map.singleton 8 (numberAged 0 ages)
    ]
  where
    numberAged age = fromMaybe 0 . Map.lookup age

solve :: [Int] -> (Integer, Integer)
solve = (sum . (!! 80) &&& sum . (!! 256)) . iterate step . agesMap

parserInput :: P.Parsec Void String [Int]
parserInput = P.many (P.decimal <* (P.char ',' P.<|> P.newline))

main :: IO ()
main = do
  input <- getContents
  case P.parse parserInput "stdin" input of
    Left bundle -> putStr (P.errorBundlePretty bundle)
    Right ages -> print (solve ages)
