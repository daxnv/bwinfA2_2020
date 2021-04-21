{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Extract where

import Types
import Data.Bifunctor (bimap, second)
import Data.List (sort, mapAccumR)
import Data.Maybe (catMaybes)

class Overlap a where
  overlap :: a -> a -> (a,a,a)

instance (Ord a) => Overlap [a] where
  overlap ls [] = (ls,[],[])
  overlap [] rs = ([],[],rs)
  overlap xxs@(x:xs) yys@(y:ys) =
    case compare x y of
      LT -> let (ls,os,rs) = overlap xs yys in (x:ls,os,rs)
      EQ -> let (ls,os,rs) = overlap xs  ys in (ls,x:os,rs)
      GT -> let (ls,os,rs) = overlap xxs ys in (ls,os,y:rs)

instance Overlap Spit where
  overlap l r = ((lq,lf), (q,f), (rq,rf))
    where
      (lq,q,rq) = overlap (queues l) (queues r)
      (lf,f,rf) = overlap (fruits l) (fruits r)

extract :: [Fruit] -> [Spit] -> ([Fruit],[([Fruit],[Fruit],[Queue])])
extract = second catMaybes ... mapAccumR helper
  where
    (...) = (.).(.)
    helper wish spit = let (unwanted,full,unfull) = overlap (fruits spit) wish
                           info
                            | null full = Nothing
                            | otherwise = Just (full,unwanted,queues spit)
                       in (unfull,info)

sortInput :: (Int, [Fruit], [Spit]) -> (Int, [Fruit], [Spit])
sortInput = bimap sort (fmap sortSpit)
  where
    sortSpit = bimap sort sort

getUnknown :: Int -> [Int] -> [Int]
getUnknown num obs = filter (not . (`elem` obs)) [1..num]

addUnknown :: [Queue] -> ([Fruit], [([Fruit], [Fruit], [Queue])]) -> [([Fruit], [Fruit], [Queue])]
addUnknown qs (fs,xs)
  | null fs   = xs
  | diff == 0 = (fs,[],qs) : xs
  | otherwise = (fs,["unbekannt"],qs) : xs
  where
    diff = length qs - length fs

 {-
instance Overlap (V.Vector a) where
  overlap = con . unzip3 ... h
    where
      infix 8 ...
      (...) = (.).(.)
      con (xs,ys,zs) = (V.concat xs, V.concat ys, V.concat zs)
      h xs ys
        | V.null xs || V.null ys = [(xs, V.empty, ys)]
        | otherwise = let x = V.head xs; y = V.head ys in
            case compare x y of
              LT -> let (lt,eq) = V.span (< y) xs in (lt,V.empty,V.empty) : h eq ys
              EQ ->                                   (V.empty,V.singleton x,V.empty) : h (V.tail xs) (V.tail ys)
              GT -> let (lt,eq) = V.span (< x) ys in (V.empty,V.empty,lt) : h xs eq
-}