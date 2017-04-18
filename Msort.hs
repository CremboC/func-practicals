module Msort where

import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.FitSpec

-- toplevel function

msort :: Ord a => [a] -> [a]
msort = pairwise [] . units

-- pairwise merging

pairwise :: Ord a => [[a]] -> [[a]] -> [a]
pairwise []  []            = []
pairwise []  [xs]          = xs
pairwise yss (xs1:xs2:xss) = pairwise ((xs1 `merge` xs2) : yss) xss
pairwise yss xss           = pairwise [] (xss ++ reverse yss)

-- merging two sorted lists

merge :: Ord a => [a] -> [a] -> [a]
merge []     []     = []
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys

-- divide into unit lists

units :: [a] -> [[a]]
units xs = map (:[]) xs

prop_unitsZero :: [Word2] -> Bool
prop_unitsZero xs = length xs == 0 ==> units xs == []

prop_units :: [Word2] -> Bool
prop_units xs = length xs > 0 ==> length (units xs) == length xs

prop_mergeLE :: [Word2] -> Bool
prop_mergeLE ys = length ys > 0 ==> merge [] ys == ys

prop_mergeRE :: [Word2] -> Bool
prop_mergeRE xs = length xs > 0 ==> merge xs [] == xs

prop_merge :: [Word2] -> [Word2] -> Bool
prop_merge xs ys = length xs > 0 && length ys > 0 ==> length (merge xs ys) == length xs + length ys

prop_msortEmpty :: [Word2] -> Bool
prop_msortEmpty xs = length xs == 0 ==> msort xs == []

prop_msort :: [Word2] -> Bool
prop_msort xs = length xs > 0 ==> f (msort xs)
	where
		f [a]      = True
		f (a:b:xs) = a <= b && f (b:xs)