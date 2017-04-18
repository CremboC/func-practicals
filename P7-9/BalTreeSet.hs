module BalTreeSet (Set,module SetSpec) where

import SetSpec
import Prelude hiding (min)

type Set a = BalTree a

data BalTree a  =  Null
                |  Node Int (BalTree a) a (BalTree a)

type SmartNode a  =  (BalTree a, a, BalTree a) -> BalTree a

-- size-based smart construction

node :: SmartNode a
node (x,a,y)  =  Node (size x + 1 + size y) x a y

rotL1 :: SmartNode a
rotL1 (x,a,Node _ y b z)  =  node(node(x,a,y),b,z)

rotL2 :: SmartNode a
rotL2 (x,a,Node _ y c z)  =  node(node(x,a,y1),b,node(y2,c,z))
  where
  Node _ y1 b y2  =  y

rotR1 :: SmartNode a
rotR1 (Node _ x a y,b,z)  =  node(x,a,node(y,b,z))

rotR2 :: SmartNode a
rotR2 (Node _ x a y,c,z)  =  node(node(x,a,y1),b,node(y2,c,z))
  where
  Node _ y1 b y2  =  y

bal :: SmartNode a
bal xay@(x,a,y)
  |  xn+yn < 2        =  node xay
  |  yn > ratio * xn  =  if size yl < size yr
                         then rotL1 xay else rotL2 xay
  |  xn > ratio * yn  =  if size xr < size xl
                         then rotR1 xay else rotR2 xay
  |  otherwise        =  node xay
  where
  xn                  =  size x
  yn                  =  size y
  Node _ xl _ xr      =  x
  Node _ yl _ yr      =  y

ratio :: Int 
ratio  =  5

balance :: Ord a => SmartNode a
balance (Null,a,x)   =  add a x
balance (x,a,Null)   =  add a x
balance xby@(x,b,y)  |  ratio * nx < ny  =  bal(balance(x,b,yl),c,yr)
                     |  ratio * ny < nx  =  bal(xl,a,balance(xr,b,y))
                     |  otherwise        =  node xby
  where
  Node nx xl a xr  =  x
  Node ny yl c yr  =  y

-- precondition: all items in 1st arg < all items in 2nd arg
cat :: Ord a => BalTree a -> BalTree a -> BalTree a
cat Null x     =  x
cat x    Null  =  x
cat x    y     =  balance(x,min y,delMin y)

min :: BalTree a -> a
min (Node _ Null a _)    =  a
min (Node _ x    _ _)    =  min x

delMin :: BalTree a -> BalTree a
delMin (Node _ Null _ x)  =  x
delMin (Node _ x    a y)  =  bal(delMin x,a,y)

before :: Ord a => BalTree a -> a -> BalTree a
Null         `before` _  =  Null
Node _ x a y `before` b  =  case compare b a of
                            LT -> x `before` b
                            EQ -> x
                            GT -> balance(x,a,y `before` b)

beyond :: Ord a => BalTree a -> a -> BalTree a
Null         `beyond` _  =  Null
Node _ x a y `beyond` b  =  case compare b a of
                            LT -> balance(x `beyond` b,a,y)
                            EQ -> y
                            GT -> y `beyond` b

instance SetSpec BalTree where

  empty                    =  Null

  singleton a              =  Node 1 Null a Null

  size Null                =  0
  size (Node n _ _ _)      =  n

  member _ Null            =  False
  member a (Node _ x b y)  =  case compare a b of
                              LT -> member a x
                              EQ -> True
                              GT -> member a y

  add a Null               =  singleton a
  add a x@(Node _ y b z)   =  case compare a b of
                              LT -> bal(add a y,b,z)
                              EQ -> x
                              GT -> bal(y,b,add a z)

  delete _ Null            =  Null
  delete a (Node _ x b y)  =  case compare a b of
                              LT -> bal(delete a x,b,y)
                              EQ -> cat x y
                              GT -> bal(x,b,delete a y)

  Null `union` x             =  x
  x    `union` Null          =  x
  x    `union` Node _ y b z  =  balance(y',b,z')
                                where
                                y'  =  (x `before` b) `union` y
                                z'  =  (x `beyond` b) `union` z

  inter                      =  error "Exercise: define inter in BalTreeSet"

  diff                       =  error "Exercise: define diff in BalTreeSet"

  elements t                 =  f t []
    where  f Null            =  id
           f (Node _ x a y)  =  f x . (a :) . f y

instance Show a => Show (BalTree a) where
	show = showSet