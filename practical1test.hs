module Practical1Test where

import Practical1
import Prelude hiding (take, drop)
import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.FitSpec

between :: Int -> Int -> Int -> Bool
between val bot top = val > bot && val < top

test_takeZero :: Bool
test_takeZero = take 0 [1..5] == [] 

test_takeWhole :: Bool
test_takeWhole = take 10 [1..5] == [1..5]

prop_take :: Int -> [Int] -> Bool
prop_take n xs = n > length xs 
                    ==> take n xs == xs

prop_take0 :: [Int] -> Bool
prop_take0 xs = take 0 xs == []

prop_takeSome :: Int -> [Int] -> Bool
prop_takeSome n xs = between n 0 (length xs)
                    ==> length (take n xs) == n

prop_drop0 :: [Int] -> Bool
prop_drop0 xs = drop 0 xs == xs

prop_dropWhole :: Int -> [Int] -> Bool
prop_dropWhole n xs = n > length xs 
                    ==> drop n xs == []

prop_dropSome :: Int -> [Int] -> Bool
prop_dropSome n xs = between n 0 (length xs) 
                    ==> length (drop n xs) == length xs - n
                    
properties_take :: (Int -> [Word2] -> [Word2]) -> [Property]
properties_take take = 
    [ property $ \xs   -> take 0 xs == []
    , property $ \n xs -> n >= length xs ==> take n xs == xs
    , property $ \n xs -> between n 0 (length xs) ==> length (take n xs) == n
	, property $ \n xs -> n < 0 ==> take n xs == []
    ]
    
    
main = reportWith args { names = ["take n xs"] }
            take
            properties_take
            