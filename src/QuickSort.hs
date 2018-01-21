module QuickSort where
import Control.Parallel (par, pseq)

-- sequential quicksort
seqSort :: Ord a => [a] -> [a]
seqSort []     = []
seqSort (x:xs) = (seqSort lesser) ++ [x] ++ (seqSort greater)
    where
        lesser  = filter (< x) xs
        greater = filter (>= x) xs

-- parallel quicksort
parSort :: (Ord a) => [a] -> [a]
parSort []      = []
parSort (x:xs)  = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where 
        lesser  = parSort [y | y <- xs, y <  x]
        greater = parSort [y | y <- xs, y >= x]

-- parallel quicksort optimized
parSortO :: (Ord a) => Int -> [a] -> [a]
parSortO _ [] = []
parSortO d list@(x:xs)
  | d <= 0    = seqSort list
  | otherwise = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where 
        lesser  = parSortO dn [y | y <- xs, y <  x]
        greater = parSortO dn [y | y <- xs, y >= x]
        dn = d - 1

force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1