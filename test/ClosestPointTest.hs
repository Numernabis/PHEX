module ClosestPointTest where

import ClosestPoint
import Test.HUnit



main = runTestTT tests

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

test1 = TestCase (assertEqual "(1,1)" (1,1) (closest (1,1) list))

test2 = TestCase (assertEqual "(55,3)" (55,3) (closest (55,2) list))

list = [(1,1),(15,17),(23,46),(55,3),(12,43),(0,0),(1,43),(2,85),(11,12),(12,1),(1,2),(12,143),(10,10)]
