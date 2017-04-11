module Practical1 where

import Prelude hiding (take, drop, zipWith)

take, drop :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x : take (n - 1) xs 

drop _ [] = []
drop 0 xs = xs
drop n (x:xs) = drop (n - 1) xs

positions :: Eq a => [a] -> a -> [Int]
positions xs i = [e |(x, e) <- zxs, x == i]
	where zxs = zip xs [0..]
	
duplicates :: Eq a => [a] -> [a]
duplicates xs = f xs []
	where f (x:xs) es | x `notElem` es && x `elem` xs = f xs (x : es)
	                  | otherwise                     = f xs es
	      f [] es = es 
	
zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

data Mat a = Mat [[a]]

instance Show a => Show (Mat a) where
	show (Mat rs) = unlines $ map f rs
		where f r = unwords $ map show r
		
transpose :: Mat a -> Mat a
transpose (Mat rs) = Mat (map f [0..length (head rs) - 1])
	where f n = [r !! n | r <- rs]
	
data Tri a = Tri [[a]]
instance Show a => Show (Tri a) where
	show (Tri rs) = unlines $ row 0 rs
		where row n (r:rs) = (pad n ++ f r) : row (succ n) rs
		      row _ [] = []
		      pad n = replicate (length rs - (n + 1)) ' '
		      f r = unwords $ map show r