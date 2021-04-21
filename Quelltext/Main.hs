module Main where

import Parser
import Lib
import Composer
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

computation :: T.Text -> Either String T.Text
computation file = do
  parse <- parseFile file
  result <- makeDonaldHappy parse
  return $ composeOutput result

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Erwarte Dateiname als Parameter"
    (name:_) -> do
      file <- T.readFile name
      let result = computation file
      case result of
        Left msg -> putStrLn msg
        Right res -> T.putStrLn res
