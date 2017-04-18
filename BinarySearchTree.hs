module BinarySearchTree where

import Test.LeanCheck
import TreeSet

-- data Tree a = Null | Node (Tree a) a (Tree a) deriving Show

searchTreeInvariant :: Ord a => Tree a -> Bool
searchTreeInvariant Null = True
searchTreeInvariant (Node Null _ Null) = True
searchTreeInvariant (Node Null n rt@(Node _ r _)) = 
	r > n && searchTreeInvariant rt
searchTreeInvariant (Node lt@(Node _ l _) n Null) = 
	l < n && searchTreeInvariant lt
searchTreeInvariant (Node lt@(Node _ l _) n rt@(Node _ r _)) = 
	l < n && r > r && searchTreeInvariant lt && searchTreeInvariant rt
	
	
instance (Ord a, Listable a) => Listable (Tree a) where
	tiers = cons0 Null \/ (cons3 Node `suchThat` searchTreeInvariant)
	
invariantPreserving :: (Ord a) =>
						(Tree a -> Tree a -> Tree a)
						-> Tree a
						-> Tree a
						-> Bool
invariantPreserving op t1 t2 = 
	searchTreeInvariant t1 && searchTreeInvariant t2 ==> searchTreeInvariant (t1 `op` t2 )
	
	
-- just attempting listable	for something different
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp
    deriving Show
    
type Name = String

instance Listable Exp where
	tiers = cons0 Nil \/ cons1 Var \/ cons2 App \/ cons2 Cons