{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Foldable (foldl')
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P hiding (space)

data Command = Forward Integer | Down Integer | Up Integer

data Position = Position {horiz :: Integer, depth :: Integer, aim :: Integer}

origin :: Position
origin = Position 0 0 0

move1 :: Command -> Position -> Position
move1 (Forward x) (Position h d a) = Position (h + x) d a
move1 (Down x) (Position h d a) = Position h (d + x) a
move1 (Up x) (Position h d a) = Position h (d - x) a

move2 :: Command -> Position -> Position
move2 (Forward x) (Position h d a) = Position (h + x) (d + a * x) a
move2 (Down x) (Position h d a) = Position h d (a + x)
move2 (Up x) (Position h d a) = Position h d (a - x)

solve :: (Command -> Position -> Position) -> [Command] -> Integer
solve move = (\case Position h d _ -> h * d) . foldl' (flip move) origin

type Parser = P.Parsec Void String

parserInput :: Parser [Command]
parserInput =
  P.many $
    P.choice [Forward <$> pAux "forward", Down <$> pAux "down", Up <$> pAux "up"]
      <* P.newline
  where
    pAux :: String -> Parser Integer
    pAux str = P.string str *> P.space *> P.decimal

main :: IO ()
main = do
  input <- getContents
  case P.parse parserInput "stdin" input of
    Left bundle -> putStr (P.errorBundlePretty bundle)
    Right commands -> print (solve move1 commands, solve move2 commands)
