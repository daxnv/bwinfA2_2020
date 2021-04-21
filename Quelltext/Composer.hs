{-# LANGUAGE OverloadedStrings #-}

module Composer (composeOutput) where

import Types
import qualified Data.Text as T
import Data.String

composeList :: (IsString p, Semigroup p) => [p] -> p
composeList [] = ""
composeList (x:xs) = x <> appendix xs
  where
    appendix [] = ""
    appendix [y] = " und " <> y
    appendix (y:ys) = ", " <> y <> appendix ys

composeInfo :: ([Fruit],[Fruit],[Queue]) -> T.Text
composeInfo (fruits,xfruits,queues) = composeList fruits <> unwanted <> " > " <> T.pack (composeList (fmap show queues))
  where
    unwanted
      | null xfruits = ""
      | otherwise      = " | " <> composeList xfruits

composeOutput :: [([Fruit],[Fruit],[Queue])] -> T.Text
-- Nimmt die Wünsche und eingegrenzte Gruppen
composeOutput = T.unlines . fmap composeInfo
