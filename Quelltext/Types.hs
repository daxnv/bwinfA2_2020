module Types where

import Data.Text

type Queue = Int
type Fruit = Text
type Spit = ([Queue], [Fruit])

queues :: Spit -> [Queue]
queues = fst

fruits :: Spit -> [Fruit]
fruits = snd
