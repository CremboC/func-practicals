-- Example Solutions for FUNC Formative Test

import Test.LeanCheck
import Test.FitSpec
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, inits, tails)

-- GIVEN: DUPLICATES AND COUNT

duplicates :: Eq a => [a] -> [a]
duplicates []      =  []
duplicates (x:xs)  =  if x `notElem` d && x `elem` xs then x:d else d 
  where
  d  =  duplicates xs

count :: Eq a => a -> [a] -> Int
count _ []      =  0
count x (y:ys)  =  if x == y then 1 + c else c
  where
  c  =  count x ys

-- PROPERTIES ABOUT COUNT AND DUPLICATES

prop_countdup0 :: Int -> [Int] -> Bool
prop_countdup0 x xs  =  count x xs <= 1 ==> count x (duplicates xs) == 0

prop_countdup1 :: Int -> [Int] -> Bool
prop_countdup1 x xs  =  count x xs >  1 ==> count x (duplicates xs) == 1

-- PROPERTY ABOUT ORDER OF DUPLICATES

prop_duporder :: [Int] -> Bool
prop_duporder xs  =  ascendingBy (`penPos` xs) (duplicates xs)
	
ascendingBy :: Ord b => (a-> b) -> [a] -> Bool
ascendingBy f (x:y:etc)  =  f x < f y && ascendingBy f (y:etc)
ascendingBy _ _          =  True

-- penPos x xs gives the position in xs of the penultimate occurrence of x
-- precondition: count x xs > 1
penPos :: Eq a => a -> [a] -> Int
penPos x ys  =  last (init [p | (p,y) <- zip [1..] ys, y == x])

-- SAME PROPERTIES, BUT PACKAGED FOR FITSPEC

dupProps :: ([Word2] -> [Word2]) -> [Property]
dupProps duplicates  =
  [ property $ \x xs -> count x xs <= 1 ==> count x (duplicates xs) == 0
  , property $ \x xs -> count x xs >  1 ==> count x (duplicates xs) == 1
  , property $ \  xs -> ascendingBy (`penPos` xs) (duplicates xs)
  ]

main :: IO ()
main  =  mainWith args {names = ["duplicates xs"]}
                  duplicates
                  dupProps

-- A LAZY DUPLICATES

lazyDuplicates :: Eq a => [a] -> [a]
lazyDuplicates       =  lazyD []
  where
  lazyD _    []      =  []
  lazyD seen (x:xs)  |  x `elem` seen
                     =  x : lazyD seen (filter (/= x) xs)
                     |  otherwise
                     =  lazyD (x:seen) xs

-- PROPERTY ABOUT ORDER OF LAZY DUPLICATES

prop_lazyduporder :: [Int] -> Bool
prop_lazyduporder xs  =  ascendingBy (`sndPos` xs) (lazyDuplicates xs)

-- sndPos x xs gives the position in xs of the second occurrence of x
-- precondition: count x xs > 1
sndPos :: Eq a => a -> [a] -> Int
sndPos x ys  =  head (tail [p | (p,y) <- zip [1..] ys, y == x])

-- SEGSINCXT: SEGMENTS IN CONTEXT

segsInCxt :: [a] -> [([a],[a],[a])]
segsInCxt xs   =  [ (pre,seg,suf)
                  | (pre      ,ss ) <- splits xs,
                    (seg@(_:_),suf) <- splits ss ]

-- splits xs gives all ways of splitting xs into a (prefix,suffix) pair
splits :: [a] -> [([a],[a])]
splits []      =  [([],[])]
splits (x:xs)  =  ([],x:xs) : [(x:pre,suf) | (pre,suf) <- splits xs]

--------------------------------------------------------------------

-- ALSO, NOT required as part of test answers, but for your interest:

-- PROPERTIES TO EVALUATE SEGSINCXT SOLUTIONS
-- The last three properties provide a complete specification.
-- The earlier properties are to evaluate partly-right solutions.

prop_sic_base0 :: Bool
prop_sic_base0  =  segsInCxt ([]::[Int]) == []

prop_sic_base1 :: Bool
prop_sic_base1  =  segsInCxt [0] == [([],[0],[])]

prop_sic_pre_sound :: [Int] -> Bool
prop_sic_pre_sound xs  =
  all (\(as,_,_) -> as `isPrefixOf` xs) (segsInCxt xs)

prop_sic_pre_complete :: [Int] -> Bool
prop_sic_pre_complete xs  =
  not (null xs) ==> all (`elem` [as | (as,_,_) <- segsInCxt xs]) (init (inits xs))

prop_sic_seg_sound :: [Int] -> Bool
prop_sic_seg_sound xs  = 
  all (\(_,bs,_) -> not (null bs) && (bs `isInfixOf` xs)) (segsInCxt xs)

prop_sic_seg_complete :: [Int] -> Bool
prop_sic_seg_complete xs =
  all (`elem` [bs | (_,bs,_) <- segsInCxt xs]) [t | i <- inits xs, t@(_:_) <- tails i]

prop_sic_suf_sound :: [Int] -> Bool
prop_sic_suf_sound xs  =
  all (\(_,_,cs) -> cs `isSuffixOf` xs) (segsInCxt xs)

prop_sic_suf_complete :: [Int] -> Bool
prop_sic_suf_complete xs  =
  not (null xs) ==> all (`elem` [cs | (_,_,cs) <- segsInCxt xs]) (tail (tails xs))

prop_sic_no_dups :: [Int] -> Bool
prop_sic_no_dups xs  =  null (duplicates (segsInCxt xs))

prop_sic_sound :: [Int] -> Bool
prop_sic_sound xs  =
  all (\(as,bs,cs) -> not (null bs) && as++bs++cs == xs) (segsInCxt xs)

prop_sic_complete :: [Int] -> [Int] -> [Int] -> Bool
prop_sic_complete as bs cs  =
  not (null bs) ==> (as,bs,cs) `elem` segsInCxt (as++bs++cs)

checkSicFor :: Int -> IO ()
checkSicFor n  =  do
  checkFor n prop_sic_base0
  checkFor n prop_sic_base1
  checkFor n prop_sic_pre_sound
  checkFor n prop_sic_pre_complete
  checkFor n prop_sic_seg_sound
  checkFor n prop_sic_seg_complete
  checkFor n prop_sic_suf_sound
  checkFor n prop_sic_suf_complete
  checkFor n prop_sic_no_dups
  checkFor n prop_sic_sound
  checkFor n prop_sic_complete
