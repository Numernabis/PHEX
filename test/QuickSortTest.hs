module QuicksortTest where

import QuickSort
import Test.HUnit
--import Test.QuickCheck

testsQS = [
    TestLabel "test1" test1, 
    TestLabel "test2" test2,
    TestLabel "test3" test3, 
    TestLabel "test4" test4,
    TestLabel "test5" test5 ]

test1 = TestCase (assertEqual "seqSort" 560 (sum (seqSort list)))
test2 = TestCase (assertEqual "parSort" 560 (sum (parSort  list)))
test3 = TestCase (assertEqual "optParSort" 560 (sum (optParSort 2 list)))

test4 = TestCase (assertEqual "seq&par" (sum (seqSort list)) (sum (parSort  list)))
test5 = TestCase (assertEqual "par&optPar" (sum (parSort  list)) (sum (optParSort 2 list)))

list = [1,15,17,23,46,55,3,12,43,0,1,43,2,85,11,12,12,1,1,2,12,143,10,10]
