module ListSet (Set,module SetSpec,match,addOrUpdate) where

import SetSpec

type Set a  =  [a]

instance SetSpec [] where

  empty              =  []

  singleton a        =  [a]

  size               =  length

  member _ []        =  False
  member a (b:x)     =  case compare a b of
                        LT -> False
                        EQ -> True
                        GT -> member a x

  add                =  error "Exercise: define add in ListSet"

  delete             =  error "Exercise: define delete in ListSet"

  []       `union` x         =  x
  x        `union` []        =  x
  ax@(a:x) `union` by@(b:y)  =  case compare a b of
                                LT -> a : (x  `union` by)
                                EQ -> a : (x  `union` y )
                                GT -> b : (ax `union` y )
                                   
  []       `inter` _         =  []
  _        `inter` []        =  []
  ax@(a:x) `inter` by@(b:y)  =  case compare a b of
                                LT -> x  `inter` by
                                EQ -> a : (x `inter` y)
                                GT -> ax `inter` y

  []       `diff` _          =  []
  x        `diff` []         =  x
  ax@(a:x) `diff` by@(b:y)   =  case compare a b of
                                LT -> a : (x `diff` by)
                                EQ -> x  `diff` y
                                GT -> ax `diff` y

  elements                   =  id

-- precondition: 1st arg `member` 2nd
match :: Ord a => a -> Set a -> a
match a (b:x)             =  case compare a b of
                             EQ -> b
                             GT -> match a x

-- precondition: u x == x for all x
addOrUpdate :: Ord a => a -> (a->a) -> Set a -> Set a
addOrUpdate a u []        =  [a]
addOrUpdate a u bx@(b:x)  =  case compare a b of
                             LT -> a   : bx
                             EQ -> u b : x
                             GT -> b   : addOrUpdate a u x