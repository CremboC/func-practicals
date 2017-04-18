module Nsort where

import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.FitSpec

msort :: Ord a => [a] -> [a]
msort = pairwise [] . ascending

pairwise :: Ord a => [[a]] -> [[a]] -> [a]
pairwise []  []            = []
pairwise []  [xs]          = xs
pairwise yss (xs1:xs2:xss) = pairwise ((xs1 `merge` xs2) : yss) xss
pairwise yss xss           = pairwise [] (xss ++ reverse yss)

merge :: Ord a => [a] -> [a] -> [a]
merge []     []     = []
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys

ascending :: Ord a => [a] -> [[a]]
ascending [] = []
ascending xs = ys : ascending zs
 where
  (ys,zs) =
    case ascends (<=) xs of 
        ([_],_)        -> let (d, r) = ascends (>=) xs in (reverse d, r)
        yszs           -> yszs

ascends :: (a -> a -> Bool) -> [a] -> ([a],[a])
ascends prec (x:y:xs)
  | prec x y  = let (ys,zs) = ascends prec (y:xs) in (x:ys,zs) 
  | otherwise = ([x], y:xs)               
ascends _ xs  = (xs,[])

prop_ascendingE :: [Word2] -> Bool
prop_ascendingE xs = length xs == 0 ==> ascending xs == []

prop_mergeLE :: [Word2] -> Bool
prop_mergeLE ys = length ys > 0 ==> merge [] ys == ys

prop_mergeRE :: [Word2] -> Bool
prop_mergeRE xs = length xs > 0 ==> merge xs [] == xs

prop_merge :: [Word2] -> [Word2] -> Bool
prop_merge xs ys = length xs > 0 && length ys > 0 ==> length (merge xs ys) == length xs + length ys

prop_msortEmpty :: [Word2] -> Bool
prop_msortEmpty xs = length xs == 0 ==> msort xs == []

prop_msort :: [Word2] -> Bool
prop_msort xs = length xs > 0 ==> ordered (msort xs)
    where
        ordered [a]      = True
        ordered (a:b:xs) = a <= b && ordered (b:xs)
		
t xs = case xs of 
        ([_],_)        -> True
        yszs           -> False
