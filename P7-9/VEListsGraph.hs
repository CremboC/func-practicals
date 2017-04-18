module VEListsGraph (Graph, module GraphSpec) where

import GraphSpec

-- invariant for a graph VE vs es:
-- 1. vs is ordered and duplicate free
-- 2. es is ordered and duplicate free
-- 2. (v1,v2) in es ==> v1 in vs, v2 in vs
-- 3. (v1,v2) in es ==> (v2,v1) in es

data VEListsGraph a  =  VE [a] [(a,a)]

type Graph a  =  VEListsGraph a

instance GraphSpec VEListsGraph where

  empty                     =  VE []  []

  addVert v (VE vs es)      =  VE vs' es
    where              vs'  =  insert v vs

  addEdge v1 v2 (VE vs es)  =  VE vs' es'
    where              vs'  =  insert v1 (insert v2 vs)
                       es'  =  insert (v1,v2) (insert (v2,v1) es)

  delVert v (VE vs es)      =  VE vs' es'
    where              vs'  =  delete v vs
                       es'  =  [e | e@(v1,v2) <- es, v1 /= v, v2 /= v]

  delEdge v1 v2 (VE vs es)  =  VE vs  es'
    where              es'  =  delete (v1,v2) (delete (v2,v1) es)

  vertices (VE vs _)        =  vs

  adj v (VE _ es)           =  map snd $ takeWhile ((== v) . fst) $
                                         dropWhile ((<  v) . fst) $ es

insert :: Ord a => a -> [a] -> [a]
insert a []        =  [a]
insert a bx@(b:x)  =  case compare a b of
                      LT -> a : bx
                      EQ -> bx
                      GT -> b : insert a x

delete :: Ord a => a -> [a] -> [a]
delete _ []        =  []
delete a bx@(b:x)  =  case compare a b of
                      LT -> bx
                      EQ -> x
                      GT -> b : delete a x
					  
					  
instance Show a => Show (VEListsGraph a) where
	show = showGraph