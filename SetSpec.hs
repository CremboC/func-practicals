module SetSpec where

import Data.List (intersperse)

class SetSpec s where
  empty     :: s a
  singleton :: a -> s a
  add       :: Ord a => a -> s a -> s a
  delete    :: Ord a => a -> s a -> s a
  size      :: s a -> Int
  member    :: Ord a => a -> s a -> Bool
  union     :: Ord a => s a -> s a -> s a
  inter     :: Ord a => s a -> s a -> s a
  diff      :: Ord a => s a -> s a -> s a
  elements  :: s a -> [a]

set :: (Ord a, SetSpec s) => [a] -> s a
set  =  foldr add empty

showSet :: (Show a, SetSpec s) => s a -> String
showSet s  = concat $
             ["{"] ++ intersperse "," (map show (elements s)) ++ ["}"]
