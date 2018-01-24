module Fibonacci where
    
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Control.Parallel (par, pseq)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

parFib :: Integer -> Integer
parFib 0 = 0
parFib 1 = 1
parFib n = par nf (parFib (n - 2) + nf)
    where nf = parFib(n - 1)

parFib' :: Integer -> Integer
parFib' 0 = 0
parFib' 1 = 1
parFib' n = par nf1 (pseq nf2 (nf1 + nf2))
    where nf1 = parFib'(n - 1)
          nf2 = parFib'(n - 2)

parFib'' :: Integer -> Integer -> Integer
parFib'' 0 n = fib n
parFib'' _ 0 = 0
parFib'' _ 1 = 1
parFib'' d n = par nf1 (pseq nf2 (nf1 + nf2))
    where nf1 = parFib'' (d - 1) (n - 1)
          nf2 = parFib'' (d - 1) (n - 2)


fibonacci = do
    let noEl = 35
    putStrLn $ "Counting " ++ show (noEl) ++ "th element of Fibonacci sequence:\n"

    start1 <- getCurrentTime
    let fib1 = fib noEl
    putStrLn $ "fib(" ++ show (noEl) ++ ") = " ++ show (fib1)
    end1 <- getCurrentTime
    putStrLn $ show (end1 `diffUTCTime` start1) ++ " elapsed.\n"

    start2 <- getCurrentTime
    let fib2 = parFib noEl
    putStrLn $ "parFib(" ++ show (noEl) ++ ") = " ++ show (fib2)
    end2 <- getCurrentTime
    putStrLn $ show (end2 `diffUTCTime` start2) ++ " elapsed.\n"

    start3 <- getCurrentTime
    let fib3 = parFib' noEl
    putStrLn $ "parFib'(" ++ show (noEl) ++ ") = " ++ show (fib3)
    end3 <- getCurrentTime
    putStrLn $ show (end3 `diffUTCTime` start3) ++ " elapsed.\n"

    start4 <- getCurrentTime
    let fib4 = parFib'' 3 noEl
    putStrLn $ "parFib'' 3 (" ++ show (noEl) ++ ") = " ++ show (fib4)
    end4 <- getCurrentTime
    putStrLn $ show (end4 `diffUTCTime` start4) ++ " elapsed.\n"

-- -----------------------------------------------------------