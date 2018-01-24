module FibonacciTest where

import Fibonacci
import Test.HUnit


testsFib = [
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
    TestLabel "test12" test12 ]

test1 = TestCase (assertEqual "fib" 514229 (fib 29))
test2 = TestCase (assertEqual "fib" 2178309 (fib 32))
test3 = TestCase (assertEqual "fib" 9227465 (fib 35))

test4 = TestCase (assertEqual "parFib" 514229 (parFib 29))
test5 = TestCase (assertEqual "parFib" 2178309 (parFib 32))
test6 = TestCase (assertEqual "parFib" 9227465 (parFib 35))

test7 = TestCase (assertEqual "parFib'" 514229 (parFib' 29))
test8 = TestCase (assertEqual "parFib'" 2178309 (parFib' 32))
test9 = TestCase (assertEqual "parFib'" 9227465 (parFib' 35))

test10 = TestCase (assertEqual "parFib''" 514229 (parFib'' 3 29))
test11 = TestCase (assertEqual "parFib''" 2178309 (parFib'' 3 32))
test12 = TestCase (assertEqual "parFib''" 9227465 (parFib'' 3 35))
