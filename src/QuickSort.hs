module QuickSort where
    
import System.Random (StdGen, getStdGen, randoms)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Control.Parallel (par, pseq)

-- | Sequential quicksort algorithm.
seqSort :: Ord a => [a] -> [a]
seqSort []     = []
seqSort (x:xs) = (seqSort lesser) ++ [x] ++ (seqSort greater)
    where
        lesser  = filter (< x) xs
        greater = filter (>= x) xs

-- | Parallel quicksort algorithm.
parSort :: (Ord a) => [a] -> [a]
parSort []      = []
parSort (x:xs)  = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where 
        lesser  = parSort [y | y <- xs, y <  x]
        greater = parSort [y | y <- xs, y >= x]

-- | Parallel quicksort algorithm - optimized.
parSortO :: (Ord a) => Int -> [a] -> [a]
parSortO _ [] = []
parSortO d list@(x:xs)
  | d <= 0    = seqSort list
  | otherwise = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where 
        lesser  = parSortO dn [y | y <- xs, y <  x]
        greater = parSortO dn [y | y <- xs, y >= x]
        dn = d - 1

-- | Dunction that forces "the entire spine of a list to be 
-- evaluated before we give back a constructor."
force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1

-- | Function to generate random list of Ints.
randomInts :: Int -> StdGen -> [Int]
randomInts k g = 
    let result = take k (randoms g)
    in force result `seq` result

-- ----------------------
-- testFunction = seqSort
-- testFunction = parSort
testFunction = parSortO 2
-- ----------------------

qs = do
    let count = 500000
    input <- randomInts count `fmap` getStdGen
    putStrLn $ "We have " ++ show (length input) ++ " elements to sort."
    start <- getCurrentTime
    let sorted = testFunction input
    putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."