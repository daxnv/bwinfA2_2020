module Groups where

import Types
import Data.List (groupBy,nub,sortBy,transpose)
import Data.Array.Unboxed
import Data.Function (on)

type Column a = (a, UArray Int Bool)
type Table a = [Column a]

makeTable :: (Foldable t, Eq a) => [a] -> [t a] -> Table a
makeTable heads rows = zip heads $ fmap (listArray (1,length heads)) $ transpose $ fmap tick rows
  where
    tick row = fmap (`elem` row) heads

groupTable :: Table a -> [Table a]
groupTable = groupBy ((==) `on` snd) . sortBy (compare `on` snd)

merge :: Table Queue -> Table Fruit -> Spit
merge qs fs = (fmap fst qs, fmap fst fs)

observed :: (Foldable t, Eq a) => t [a] -> [a]
observed = nub . concat
