# PHEX - Parallel Haskell Examples
Ludwik Ciechański & Wojciech Wańczyk

Projekt z przedmiotu *Programowanie Funkcyjne*.

Badanie czasu wykonania programów równoległych.

## Fibonacci
```haskell
fib :: Integer -> Integer
parFib :: Integer -> Integer
parFib' :: Integer -> Integer
parFib'' :: Integer -> Integer -> Integer
```
## Quicksort
```haskell
seqSort :: Ord a => [a] -> [a]
parSort :: (Ord a) => [a] -> [a]
optParSort :: (Ord a) => Int -> [a] -> [a]
```
## SumPrimes
```haskell
sumTwoPrimes :: Int -> Int -> Int
sumTwoPrimesPar :: Int -> Int -> Int
sumPrimesInRange :: [Int] -> Int
sumPrimesInRangePar :: [Int] -> Int
```
## ClosestPoint
```haskell
closest :: (Float, Float) -> [(Float, Float)] -> (Float, Float)
closestPar :: (Float, Float) -> [(Float, Float)] -> (Float, Float)
```
## ImageConversion
```haskell
toGreyScale :: Image PixelRGB8 -> Image Pixel8
```