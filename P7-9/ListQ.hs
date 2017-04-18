module ListQ (Queue, module QueueSpec) where

import QueueSpec

data ListQ a  =  LQ [a]

type Queue a  =  ListQ a

instance QueueSpec ListQ where
  empty             =  LQ []
  snoc (LQ xs) x    =  LQ (xs ++ [x])
  head (LQ (x:xs))  =  x
  tail (LQ (x:xs))  =  LQ xs
  queue             =  LQ
  items   (LQ xs)   =  xs
  isEmpty (LQ xs)   =  null xs
