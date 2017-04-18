module RotatingQ (Queue, module QueueSpec) where

import QueueSpec

data RotatingQ a  =  RQ [a] [a] [a]

type Queue a      =  RotatingQ a

rq :: [a] -> [a] -> [a] -> RotatingQ a
rq f r (x:s)  =  RQ f  r  s
rq f r []     =  RQ f' [] f'
  where   f'  =  rotate f r []

rotate :: [a] -> [a] -> [a] -> [a]
rotate []    [y]   a  =  y : a
rotate (x:f) (y:r) a  =  x : rotate f r (y:a)

instance QueueSpec RotatingQ where
  empty                =  RQ [] [] []
  isEmpty (RQ f _ _)   =  null f
  snoc (RQ f r s) x    =  rq f (x:r) s
  head (RQ (x:_) _ _)  =  x
  tail (RQ (_:f) r s)  =  rq f r s
  queue xs             =  RQ xs [] xs
  items (RQ f r _)     =  f ++ reverse r

instance Show a => Show (RotatingQ a) where
	show = showQueue