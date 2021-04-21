module Lib where

import Types
import Groups
import Extract
import Control.Applicative (liftA2)

guard :: String -> Bool -> Either String ()
guard _ True = return ()
guard ms False = Left ms

merges :: Table Queue -> Table Fruit -> Either String Spit
merges qs fs = check >> return (merge qs fs)
  where
    checkLength xs ys = length xs == length ys
    checkContent xs ys = snd (head xs) == snd (head ys)
    checkBoth = liftA2 (liftA2 (&&)) checkLength checkContent
    check = guard "Beobachtungen passen nicht zusammen" $ checkBoth qs fs

compute :: (Int, [Fruit], [Spit]) -> Either String [([Fruit], [Fruit], [Queue])]
compute (num, wish, observation) = do
  let (qObs, fObs) = unzip observation
      qUnion = observed qObs
      fUnion = observed fObs

  guard "Die Anzahlen aller beobachteten Schuesseln und Fruechte sind nicht gleich" $ length qUnion == length fUnion

  let qGroups = groupTable $ makeTable qUnion qObs
      fGroups = groupTable $ makeTable fUnion fObs
      maybeGroups = zipWith merges qGroups fGroups

  groups <- sequence maybeGroups
  return $ addUnknown (getUnknown num qUnion) $ extract wish groups

makeDonaldHappy :: (Int, [Fruit], [Spit]) -> Either String [([Fruit], [Fruit], [Queue])]
makeDonaldHappy = compute . sortInput
