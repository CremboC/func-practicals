-- Example solutions for practical exercises after L10 & L11.

import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.FitSpec
import qualified Msort
import qualified Nsort
import Parse
import Pretty
import Data.Char (isLower)

-- PROPERTIES OF TAKE AND DROP
-- As take and drop are undefined for negative first arguments,
-- we may choose to declare test properties over values of the Nat
-- type; it has values of the form (Nat n) where n is a non-negative Int.

prop_lengthTake :: Nat -> [Int] -> Bool
prop_lengthTake (Nat n) xs   =  length (take n xs) == min n (length xs)

prop_lengthDrop :: Nat -> [Int] -> Bool
prop_lengthDrop (Nat n) xs   =  length (drop n xs) == max 0 (length xs - n)

prop_catTakeDrop :: Nat -> [Int] -> Bool
prop_catTakeDrop (Nat n) xs  =  take n xs ++ drop n xs == xs

-- MUTATION TESTING BY FITSPEC
-- Using FitSpec, the search space can blow up unhelpfully if basic
-- types of things like list elements are too large.  So instead
-- of Int elements, we use the type Word2 which has just four values.

-- Also, declaring the mutant functions to have first arguments of type Nat
-- avoids wasteful undefined tests for negative arguments.  We wrap the
-- orginal take and drop accordingly in the mainWith call.

-- Finally, although I include here all three properties, FitSpec correctly
-- points out that the last property in combination with either one of the
-- first two is enough.

takeDropProperties ::
  (Nat -> [Word2] -> [Word2], Nat -> [Word2] -> [Word2]) -> [Property]
takeDropProperties (take,drop)  =
  [ property $ \n@(Nat i) xs ->  length (take n xs) == min i (length xs)
  , property $ \n@(Nat i) xs ->  length (drop n xs) == max 0 (length xs - i)
  , property $ \n@(Nat i) xs ->  take n xs ++ drop n xs == xs
  ]

main  =  mainWith args {names = ["take n xs", "drop n xs"]}
                  (\(Nat n) -> take n, \(Nat n) -> drop n)
                  takeDropProperties

-- PROPERTIES OF MERGE SORTS

-- The functions 'ordered' and 'count' will be useful in properties.

ordered :: Ord a => [a] -> Bool
ordered (x:y:etc)  =  x <= y && ordered (y:etc)
ordered _          =  True

count :: Eq a => a -> [a] -> Int
count x []         =  0
count x (y:ys)     =  (if x==y then 1 else 0) + count x ys

-- 'msort' is defined differently in Msort and Nsort
-- prop_order Msort.msort and prop_count Msort.msort test OK
-- but prop_order Nsort.msort fails: eg. Nsort.msort [1,0] = [1,0] ??

prop_order :: ([Int] -> [Int]) -> [Int] -> Bool
prop_order sort xs    =  ordered (sort xs)

prop_count :: ([Int] -> [Int]) -> [Int] -> Int -> Bool
prop_count sort xs x  =  count x (sort xs) == count x xs

-- 'merge' declarations are the same in Msort and Nsort
-- Both prop_mergeOrder and prop_mergeCount test OK.

prop_mergeOrder :: [Int] -> [Int] -> Bool
prop_mergeOrder xs ys    =
  ordered xs && ordered ys ==> ordered (xs `Msort.merge` ys)

prop_mergeCount :: [Int] -> [Int] -> Int -> Bool
prop_mergeCount xs ys x  =
  count x (xs `Msort.merge` ys) == count x xs + count x ys

-- 'pairwise' declarations are also the same in Msort and Nsort
-- Again properties can be expressed using ordered and count but
-- as the arguments are lists of lists we need 'all' and 'sum'.
-- Both properties test OK.

prop_pairwiseOrder :: [[Int]] -> [[Int]] -> Bool
prop_pairwiseOrder xss yss    =
  all ordered xss && all ordered yss ==> ordered (Msort.pairwise xss yss)

prop_pairwiseCount :: [[Int]] -> [[Int]] -> Int -> Bool
prop_pairwiseCount xss yss x  =
  count x (Msort.pairwise xss yss) == sum (map (count x) (xss ++ yss))

-- So that leaves the (deliberately!) complicated 'ascending' which
-- in Nsort replaces the simple and 'units' of Msort.
-- Indeed, prop_ascendingOrder fails: eg. Nsort.ascending [1,0] = [[1,0]] ??

prop_ascendingOrder :: [Int] -> Bool
prop_ascendingOrder xs    =
  all ordered (Nsort.ascending xs)

prop_ascendingCount :: [Int] -> Int -> Bool
prop_ascendingCount xs x  =
  sum (map (count x) (Nsort.ascending xs)) == count x xs

-- Nsort.ascending can be corrected by replacing
-- ([_],_) -> ascends (>=) xs
-- with
-- ([_],_) -> let (ys,zs) = ascends (>=) xs in (reverse ys,zs)

-- INVARIANT PROPERTIES FOR BINARY SEARCH TREES

data Tree a  =  Null |  Node (Tree a) a (Tree a)

searchTreeInvariant :: Ord a => Tree a -> Bool
searchTreeInvariant  =  sti Nothing Nothing

-- The first two arguments are optional lower and upper limits on
-- the values a tree should contain.
sti :: Ord a => Maybe a -> Maybe a -> Tree a -> Bool
sti _ _     Null            =  True
sti mlo mhi (Node t1 x t2)  =  within mlo mhi x &&
                               sti mlo (Just x) t1 && sti (Just x) mhi t2

within Nothing   Nothing   x  =  True
within Nothing   (Just hi) x  =  x <= hi
within (Just lo) Nothing   x  =  lo <= x
within (Just lo) (Just hi) x  =  lo <= x && x <= hi

invariantPreserving ::
  Ord a => (Tree a -> Tree a -> Tree a) -> Tree a -> Tree a -> Bool
invariantPreserving op t1 t2  = 
  searchTreeInvariant t1 && searchTreeInvariant t2 ==>
  searchTreeInvariant (t1 `op` t2)

instance Listable a => Listable (Tree a) where
  tiers  =  cons0 Null \/ cons3 Node

-- PARSE & PRETTY COMPATBILITY FOR SIMPLE LAMBDA EXPRESSIONS

data Expr  =  Var Char | Lam Char Expr | App Expr Expr
              deriving (Show,Eq)

-- For properties and test-data generation it is handy to
-- introduce a custom char type for lower-case letters.

data LC  =  LC Char

varLC :: LC -> Expr
varLC (LC c)    =  Var c

lamLC :: LC -> Expr -> Expr
lamLC (LC c) e  =  Lam c e

instance Listable LC where
  list  =  map LC ['a' .. 'z']

instance Listable Expr where
  tiers  =  cons1 varLC \/ cons2 lamLC \/ cons2 App

-------- parser from L5 -------
expr :: Parser Expr
expr  =  foldl1 App .:. many1 atom
     .|. Lam .:. sym "\\" *.. var ..* sym "." .*. expr

atom :: Parser Expr
atom  =  Var .:. var
     .|. sym "(" *.. expr ..* sym ")"

var :: Parser Char
var   =  token (sat isLower)
-------------------------------

------- printer from L6 -------
doc :: Expr -> DOC
doc (Var v)    =  text [v]
doc (Lam v e)  =  group (text ("\\"++[v]++".") <> 
                         nest 2 (line <> doc e))
doc (App f a)  =  group (df <>
                         nest 2 (line <> da))
  where
  df           =  if isLam f
                  then bracket "(" (doc f) ")" else doc f
  da           =  if isVar a
                  then doc a else bracket "(" (doc a) ")"

isLam (Lam _ _)  =  True
isLam _          =  False

isVar (Var _)    =  True
isVar _          =  False
--------------------------------

-- a simple round-trip check, for a fixed pretty-printing page-width
prop_parsePretty :: Expr -> Bool
prop_parsePretty e  =  expr (pretty 100 (doc e))  == [(e,"")]