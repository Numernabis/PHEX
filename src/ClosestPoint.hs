{-|
Module      : ClosestPoint
Maintainer  : wojtekwanczyk@gmail.com


-}
module ClosestPoint where

import Data.List
import Control.Parallel
import Data.Time (getCurrentTime, diffUTCTime)


-- | Compare distance between two points.
distance :: (Float, Float) -> (Float, Float) -> Float
distance a b = sqrt ((fst a - fst b)^2 + (snd a - snd b)^2)

-- | Find closest point from given pair.
minPair2 p a b 
 | distance p a < distance p b = a
 | otherwise = b

-- | Find closest point from given trio.
minPair3 p a b c
 | distance p a < distance p b && distance p a < distance p c = a
 | distance p b < distance p a && distance p b < distance p c = b
 | otherwise = c

-- | Function that finds closest point parallel.
closest :: (Float, Float) -> [(Float, Float)] -> (Float, Float)
closest p (x:y:[]) = a
 where 
  a = minPair2 p x y
closest p (x:y:z:[]) = a
 where 
  a = minPair3 p x y z
closest p list = final
 where 
  len = length list
  mid = div len 2
  l = take mid list
  r = drop mid list
  a = closest p l
  b = closest p r
  final = minPair2 p a b
  
-- | Function that finds closest point sequentially.
closestPar :: (Float, Float) -> [(Float, Float)] -> (Float, Float)
closestPar p (x:y:[]) = a
 where 
  a = minPair2 p x y
closestPar p (x:y:z:[]) = a
 where 
  a = minPair3 p x y z
closestPar p list = final
 where 
  len = length list
  mid = div len 2
  l = take mid list
  r = drop mid list
  a = closestPar p l
  b = closestPar p r
  final = par a (pseq b (minPair2 p a b))
  
-- | Main function - executes both ClosestPoint functions with time measurement.
runClosestPoint = do
 putStrLn "==========================================="
 putStrLn "Execute closest (0,0) [(x,y) | x<-[1..300000], y<-[300000, 600000]]"
 start <- getCurrentTime
 print (closest (0,0) [(x,y) | x<-[1..300000], y<-[300000, 600000]])
 end <- getCurrentTime
 putStr "Execution time:  "
 let dif1 = diffUTCTime end start
 print (dif1)
 putStrLn " "
 putStrLn "Execute closestPar (0,0) [(x,y) | x<-[1..300000], y<-[300000, 600000]]"
 startp <- getCurrentTime
 print (closestPar (0,0) [(x,y) | x<-[1..300000], y<-[300000, 600000]])
 endp <- getCurrentTime
 putStr "Execution time:  "
 let dif2 = diffUTCTime endp startp
 print (dif2)
 putStrLn " "
 putStr "Difference in execution: "
 print (dif1 - dif2)
 putStrLn "==========================================="
 putStrLn " "

