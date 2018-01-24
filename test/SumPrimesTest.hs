module SumPrimesTest where

import SumPrimes
import Test.HUnit


main = runTestTT tests
tests = TestList [
    TestLabel "test1" test1, 
    TestLabel "test2" test2,
    TestLabel "test3" test3, 
    TestLabel "test4" test4,
    TestLabel "test5" test5, 
    TestLabel "test6" test6,
    TestLabel "test7" test7, 
    TestLabel "test8" test8,
    TestLabel "test9" test9, 
    TestLabel "test10" test10,
    TestLabel "test11" test11, 
    TestLabel "test12" test12,
    TestLabel "test13" test13, 
    TestLabel "test14" test14,
    TestLabel "test15" test15, 
    TestLabel "test16" test16
]

test1 = TestCase (assertEqual "ErathoSieve" [2,3,5,7] (eS [2..10]))
test2 = TestCase (assertEqual "ErathoSieve" [17,19] (eS [15..22]))

test3 = TestCase (assertEqual "SumTwoPrimes" 0 (sumTwoPrimes 20 21))
test4 = TestCase (assertEqual "SumTwoPrimes" 29 (sumTwoPrimes 29 400))
test5 = TestCase (assertEqual "SumTwoPrimes" 36 (sumTwoPrimes 17 19))

test6 = TestCase (assertEqual "SumTwoPrimesPar" 0 (sumTwoPrimesPar 45 50))
test7 = TestCase (assertEqual "SumTwoPrimesPar" 13 (sumTwoPrimesPar 13 600))
test8 = TestCase (assertEqual "SumTwoPrimesPar" 19 (sumTwoPrimesPar 6 19))

test9 = TestCase (assertEqual "SumPrimesInRange" 5 (sumPrimesInRange [5,6]))
test10 = TestCase (assertEqual "SumPrimesInRange" 49 (sumPrimesInRange [12..20] ))
test11 = TestCase (assertEqual "SumPrimesInRange" 22 (sumPrimesInRange [17,2,3]))

test12 = TestCase (assertEqual "SumPrimesInRangePar" 0 (sumPrimesInRangePar [6,8,10,12]))
test13 = TestCase (assertEqual "SumPrimesInRangePar" 26 (sumPrimesInRangePar [2,7,6,17]))
test14 = TestCase (assertEqual "SumPrimesInRangePar" 0 (sumPrimesInRangePar [1]))

test15 = TestCase (assertEqual "InRange" (sumPrimesInRange [2,7,6,17]) (sumPrimesInRangePar [2,7,6,17]))
test16 = TestCase (assertEqual "TwoPrimes"  (sumTwoPrimes 6 19)  (sumTwoPrimesPar 6 19))