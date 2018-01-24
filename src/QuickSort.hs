module QuickSort where
    
import System.Random (StdGen, getStdGen, randoms)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Control.Parallel (par, pseq)
import Data.List
import Test.QuickCheck

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
optParSort :: (Ord a) => Int -> [a] -> [a]
optParSort _ [] = []
optParSort d list@(x:xs)
  | d <= 0    = seqSort list
  | otherwise = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where 
        lesser  = optParSort dn [y | y <- xs, y <  x]
        greater = optParSort dn [y | y <- xs, y >= x]
        dn = d - 1

-- | Function that forces "the entire spine of a list to be 
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

-- | QuickCheck functions
prop_minimum :: [Int] -> Property
prop_minimum xs = not(null xs) ==> head (seqSort xs) == minimum xs

prop_maximum :: [Int] -> Property
prop_maximum xs = not(null xs) ==> last (optParSort xs) == maximum xs

prop_ordered :: [Int] -> Bool
prop_ordered xs = ordered (parSort xs) 
    where ordered [] = True 
          ordered [x] = True 
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation :: [Int] -> Bool
prop_permutation xs = permutation xs (parSort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)


-- | Main method - executes three above versions of quicksort on list of random Ints.
-- Provides survey on time of execution of those three algorithms.
quickSort = do
    let count = 700000
    input <- randomInts count `fmap` getStdGen
    putStrLn $ "We have " ++ show (length input) ++ " elements to sort."

    start1 <- getCurrentTime
    let sorted1 = seqSort input
    putStrLn $ "Sorted using seqSort. Control sum: " ++ show (sum sorted1)
    end1 <- getCurrentTime
    putStrLn $ show (end1 `diffUTCTime` start1) ++ " elapsed.\n"

    start2 <- getCurrentTime
    let sorted2 = parSort input
    putStrLn $ "Sorted using parSort. Control sum: " ++ show (sum sorted2)
    end2 <- getCurrentTime
    putStrLn $ show (end2 `diffUTCTime` start2) ++ " elapsed.\n"

    start3 <- getCurrentTime
    let sorted3 = optParSort 3 input
    putStrLn $ "Sorted using optParSort. Control sum: " ++ show (sum sorted3)
    end3 <- getCurrentTime
    putStrLn $ show (end3 `diffUTCTime` start3) ++ " elapsed."

-- -----------------------------------------------------------