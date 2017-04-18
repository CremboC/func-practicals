module FormativeQ where

import Test.LeanCheck
import Test.FitSpec

duplicates :: Eq a => [a] -> [a]
duplicates = f []
    where 
        f dups [] = dups
        f dups (x:xs) | x `elem` xs && x `notElem` dups = f (x:dups) xs
                      | otherwise                       = f dups xs 

count :: Eq a => a -> [a] -> Int
count e []     = 0
count e (x:xs) = if e == x then 1 + next else next
    where next = count e xs
    
prop_duplicatesEmpty :: [Word2] -> Bool
prop_duplicatesEmpty xs = null xs ==> duplicates xs == []

prop_duplicates :: [Word2] -> Bool
prop_duplicates xs = (not . null $ xs) ==> checkdups dups
    where 
        dups = duplicates xs
        checkdups []                       = True
        checkdups (x:ys) | count x xs == 0 = x `notElem` dups && checkdups ys
                         | otherwise       = x `elem` dups    && checkdups ys

props_duplicates :: ([Word2] -> [Word2]) -> [Property]
props_duplicates duplicates = 
    [ property $ \x xs -> count x xs <= 1 ==> count x (duplicates xs) == 0
    , property $ \x xs -> count x xs > 1  ==> count x (duplicates xs) == 1
    ]
    
lazyDuplicates :: Eq a => [a] -> [a]
lazyDuplicates xs = f xs []
    where
        f (x:xs) seen | x `notElem` seen  = f xs (x:seen)
                      | count x seen == 1 = x : f xs (x:seen)
                      | otherwise         = f xs seen
        f [] _ = []

segsInCxt :: [a] -> [([a], [a], [a])]
segsInCxt xs = []

        
main = reportWith args { names = ["duplicates xs"] }
            lazyDuplicates
            props_duplicates