{-# LANGUAGE OverloadedStrings #-}

module Parser (parseFile) where

import Types
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Char (isAlpha)
import Control.Applicative (liftA2)

parseNum :: Parser Int
parseNum = decimal <* skipSpace

parseQueues :: Parser [Queue]
parseQueues = many1 parseNum

parseFruits :: Parser [T.Text]
parseFruits = many1 $ parseFruit <* skipSpace
  where
    parseFruit = takeWhile1 isAlpha

parseSpit :: Parser Spit
parseSpit = liftA2 (,) parseQueues parseFruits

parseSpits :: Parser [Spit]
parseSpits = do
  n <- parseNum
  count n parseSpit

parseContent :: Parser (Int, [T.Text], [Spit])
parseContent = do
  menge <- parseNum
  wunsch <- parseFruits
  spiesse <- parseSpits
  endOfInput
  return (menge, wunsch, spiesse)

parseFile :: T.Text -> Either String (Int, [Fruit], [Spit])
parseFile = handle . parse parseContent
  where
    handle (Fail rest _ msg) = Left $ unlines ["Parse-Error " ++ msg, "bei:\n" ++ Prelude.take 20 (T.unpack rest)]
    handle (Partial parser) = handle $ parser mempty
    handle (Done _ result) = Right result
