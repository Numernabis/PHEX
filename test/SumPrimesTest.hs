module SumPrimesTest where

import SumPrimes
import Test.HUnit



main = runTestTT tests
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]


test1 = TestCase (assertEqual "ErathoSieve" [2,3,5,7] (eS [2..10]))

test2 = TestCase (assertEqual "ErathoSieve" [17,19] (eS [15..22]))


test3 = TestCase (assertEqual "SumTwoPrimes" 0 (sumTwoPrimes 20 21))

test4 = TestCase (assertEqual "SumTwoPrimes" 29 (sumTwoPrimes 29 400))

test5 = TestCase (assertEqual "SumTwoPrimes" 36 (sumTwoPrimes 17 19))


test6 = TestCase (assertEqual "SumTwoPrimesPar" 0 (sumTwoPrimesPar 45 50))

test7 = TestCase (assertEqual "SumTwoPrimesPar" 13 (sumTwoPrimesPar 13 600))

test8 = TestCase (assertEqual "SumTwoPrimesPar" 19 (sumTwoPrimesPar 6 19))


test9 = TestCase (assertEqual "SumTwoPrimes" 5 (sumPrimesInRange [5,6]))

test10 = TestCase (assertEqual "SumTwoPrimes" 49 (sumPrimesInRange [12..20] ))

test11 = TestCase (assertEqual "SumTwoPrimes" 22 (sumPrimesInRange [17,2,3]))


test12 = TestCase (assertEqual "SumTwoPrimesPar" 0 (sumPrimesInRangePar [6,8,10,12]))

test13 = TestCase (assertEqual "SumTwoPrimesPar" 26 (sumPrimesInRangePar [2,7,6,17]))

test14 = TestCase (assertEqual "SumTwoPrimesPar" 0 (sumPrimesInRangePar [1]))



test15 = TestCase (assertEqual "SumTwoPrimesPar" (sumPrimesInRange [2,7,6,17]) (sumPrimesInRangePar [2,7,6,17]))

test16 = TestCase (assertEqual "SumTwoPrimesPar"  (sumTwoPrimes 6 19)  (sumTwoPrimesPar 6 19))

