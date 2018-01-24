import FibonacciTest
import QuicksortTest
import ClosestPointTest
import SumPrimesTest

import Test.HUnit

allTests = testsFib ++ testsQS ++ testsPrimes ++ testsCPoint
main :: IO Counts
main = runTestTT $ TestList allTests

