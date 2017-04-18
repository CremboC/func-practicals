import QueueSpec (QueueSpec)
import qualified QueueSpec as Q
import qualified BatchedQ as BQ

breadthFirst :: (a -> [a]) -> a -> [a]
breadthFirst b r  =  bf b [r]

bf :: (a -> [a]) -> [a] -> [a]
bf b []      =  []
bf b (x:xs)  =  x : bf b (xs ++ b x)

bfQ :: (QueueSpec q) => (a -> [a]) -> q a -> [a]
bfQ entries q | Q.isEmpty q  = []
			  | otherwise    = x : bfQ entries (foldl Q.snoc xs (entries x))
			where (x, xs) = (Q.head q, Q.tail q)


main = do
	let f n = [(2*n)+1,2*(n+1)]
	print $ take 100 . bf f $ [0]
	print $ take 100 . bfQ f $ (BQ.queue [0] :: BQ.Queue Int)