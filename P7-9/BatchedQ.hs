module BatchedQ (Queue, module QueueSpec) where

import QueueSpec

data BatchedQ a  =  BQ [a] [a]

type Queue a     =  BatchedQ a

bq :: [a] -> [a] -> BatchedQ a
bq [] r     =  BQ (reverse r) []
bq f  r     =  BQ f r

instance QueueSpec BatchedQ where
  empty              =  BQ [] []
  snoc (BQ f r) x    =  bq f (x:r)
  head (BQ (x:_) _)  =  x
  tail (BQ (_:f) r)  =  bq f r
  queue xs           =  BQ xs []
  items (BQ f r)     =  f ++ reverse r

  
instance Show a => Show (BatchedQ a) where
	show = showQueue