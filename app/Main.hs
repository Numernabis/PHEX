module Main where
import QuickSort

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Random (StdGen, getStdGen, randoms)

-- testFunction = seqSort
-- testFunction = parSort
testFunction = parSortO 2

randomInts :: Int -> StdGen -> [Int]
randomInts k g = 
    let result = take k (randoms g)
    in force result `seq` result

main = do
    let count = 500000
    input <- randomInts count `fmap` getStdGen
    putStrLn $ "We have " ++ show (length input) ++ " elements to sort."
    start <- getCurrentTime
    let sorted = testFunction input
    putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
 
