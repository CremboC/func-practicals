module TreeSet (Set,module SetSpec) where

import SetSpec
import Prelude hiding (min)

type Set a = Tree a

data Tree a  =  Null
             |  Node (Tree a) a (Tree a)

-- precondition: all items in 1st arg < all items in 2nd arg
cat :: Ord a => Tree a -> Tree a -> Tree a
cat Null x     =  x
cat x    Null  =  x
cat x    y     =  Node x (min y) (delMin y)

min :: Tree a -> a
min (Node Null a _)    =  a
min (Node x    _ _)    =  min x

delMin :: Tree a -> Tree a
delMin (Node Null _ x)  =  x
delMin (Node x    a y)  =  Node (delMin x) a y

before :: Ord a => Tree a -> a -> Tree a
Null       `before` _  =  Null
Node x a y `before` b  =  case compare b a of
                          LT -> x `before` b
                          EQ -> x
                          GT -> Node x a (y `before` b)

beyond :: Ord a => Tree a -> a -> Tree a
Null       `beyond` _  =  Null
Node x a y `beyond` b  =  case compare b a of
                          LT -> Node (x `beyond` b) a y
                          EQ -> y
                          GT -> y `beyond` b

instance SetSpec Tree where

  empty                  =  Null

  singleton a            =  Node Null a Null

  size Null              =  0
  size (Node x _ y)      =  1 + size x + size y

  member _ Null          =  False
  member a (Node x b y)  =  case compare a b of
                            LT -> member a x
                            EQ -> True
                            GT -> member a y

  add a Null             =  singleton a
  add a x@(Node y b z)   =  case compare a b of
                            LT -> Node (add a y) b z
                            EQ -> x
                            GT -> Node y b (add a z)

  delete _ Null          =  Null
  delete a (Node x b y)  =  case compare a b of
                            LT -> Node (delete a x) b y
                            EQ -> cat x y
                            GT -> Node x b (delete a y)

  Null `union` x           =  x
  x    `union` Null        =  x
  x    `union` Node y b z  =  Node y' b z'
                              where
                              y'  =  (x `before` b) `union` y
                              z'  =  (x `beyond` b) `union` z

  inter                    =  error "Exercise: define inter in TreeSet"

  diff                     =  error "Exercise: define diff in TreeSet"

  elements t               =  f t []
    where  f Null          =  id
           f (Node x a y)  =  f x . (a :) . f y

