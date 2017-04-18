module AdjSetGraph where

import GraphSpec
import ListSet as S

-- invariant for G adjs:
-- A v ws `member` adjs && w `member` ws
-- ==>
-- A w vs `member` adjs && v `member` vs

data Graph a      =  G (Set (Adj a))

data Adj a  =  A a (Set a) deriving Show

instance Eq a => Eq (Adj a) where
  A v _ == A w _  =  v == w

instance Ord a => Ord (Adj a) where
  compare (A v _) (A w _)  =  compare v w

instance GraphSpec Graph where
  empty                 =  G S.empty

  addVert v (G adjs)    =  G (add (A v S.empty) adjs)

  addEdge v w (G adjs)  =  G (addTo adjs)
    where
    addTo  =  addOrUpdate (A v (singleton w))
                          (\(A v vs) -> A v (add w vs)) .
              addOrUpdate (A w (singleton v))
                          (\(A w ws) -> A w (add v ws))

  delVert v (G adjs)  =  G (delIn (delOut adjs))
    where
    delIn    =  foldr1 (.)
                  [ addOrUpdate (A w non) (\(A w xs) -> A w (delete v xs))
                  | w <- elements ws ]
    delOut   =  delete (A v non)
    A _ ws   =  match  (A v non) adjs

  delEdge v w (G adjs)  =  G (delFrom adjs)
    where
    delFrom  =  addOrUpdate (A v non) (\(A v vs) -> A v (delete w vs)) .
                addOrUpdate (A w non) (\(A w ws) -> A w (delete v ws))
  
  vertices (G adjs)  =  [v | A v _ <- elements adjs]

  adj v (G adjs)  =  elements vs
    where A _ vs  =  match (A v non) adjs

non :: a
non  =  error "Cannot evaluate a non-value!"

instance Show a => Show (AdjSetGraph a) where
	show = showGraph
