module SumPrimes where

import Control.Parallel (par, pseq)
import Data.Time (getCurrentTime, diffUTCTime)


-- check and add numbers that are primary
-- function `par` allows to execute function prarallel

sumTwoPrimesPar :: Int -> Int -> Int
sumTwoPrimesPar x y = par a (pseq b (a + b))
 where
  a = if elem x (eS [2..x]) then x else 0 
  b = if elem y (eS [2..y]) then y else 0 
  
  
-- check and add numbers that are prime
-- this function cannot be executed parallel

sumTwoPrimes :: Int -> Int -> Int
sumTwoPrimes x y = a + b 
 where
  a = if elem x (eS [2..x]) then x else 0 
  b = if elem y (eS [2..y]) then y else 0 


-- eratosthenes sieve 

eS :: [Int] -> [Int]
eS (p : xs) = p : eS [x | x <- xs, x `mod` p /= 0]
eS [] = []


-- sum prime numbers in given range

sumPrimesInRange :: [Int] -> Int
sumPrimesInRange [] = 0
sumPrimesInRange (x:xs) = a + b
 where a = if elem x (eS [2..x]) then x else 0 
       b = (sumPrimesInRange xs)

-- sum prime numbers in given range
-- this function can be executed parallel

sumPrimesInRangePar :: [Int] -> Int
sumPrimesInRangePar [] = 0
sumPrimesInRangePar (x:xs) = par a (pseq b (a + b))
 where a = if elem x (eS [2..x]) then x else 0 
       b = (sumPrimesInRangePar xs)


-- executes all above function with time measure
runSumPrimes = do
 putStrLn "==========================================="
 putStrLn "Execute sumTwoPrimes 99839 99961"
 start <- getCurrentTime
 print (sumTwoPrimes 99839 99961)
 end <- getCurrentTime
 putStr "Execution time:  "
 let dif1 = diffUTCTime end start
 print (dif1)
 putStrLn " "
 putStrLn "Execute sumTwoPrimesPar 99839 99961"
 startp <- getCurrentTime
 print (sumTwoPrimesPar 99839 99961)
 endp <- getCurrentTime
 putStr "Execution time:  "
 let dif2 = diffUTCTime endp startp
 print (dif2)
 putStrLn " "
 putStr "Difference in execution: "
 print (dif1 - dif2)
 putStrLn "-------------------------------------------"
 putStrLn "Now execute sumPrimesInRange [1..6000]"
 start <- getCurrentTime
 print (sumPrimesInRange [1..6000])
 end <- getCurrentTime
 putStr "Execution time:  "
 let dif1 = diffUTCTime end start
 print (dif1)
 putStrLn " "
 putStrLn "Execute sumPrimesInRangePar [1..6000]"
 startp <- getCurrentTime
 print (sumPrimesInRangePar [1..6000])
 endp <- getCurrentTime
 putStr "Execution time:  "
 let dif2 = diffUTCTime endp startp
 print (dif2)
 putStrLn " "
 putStr "Difference in execution: "
 print (dif1 - dif2)
 putStrLn "==========================================="
 putStrLn " "



