module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Graphics.Gloss

import Lib
import Examples

main :: IO ()
main = do
  defaultMain (testGroup "L-systems tests" [unitTests, propertyChecks])

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "iterate gosper once"
  $ axiom (iterateLSystem 1 gosper) @?= "A-B--B+A++AA+B-"
  , testCase "iterate sierpinski once"
  $ axiom (iterateLSystem 1 sierpinski) @?= "A-B+A+B-A-BB-BB"
  , testCase "instructions of one iteration of gosper"
  $ instructions (iterateLSystem 1 gosper) @?= [Forward,TurnLeft,Forward,TurnLeft,TurnLeft,Forward,TurnRight,Forward,TurnRight,TurnRight,Forward,Forward,TurnRight,Forward,TurnLeft]
  , testCase "instructions of one iteration of sierpinski"
  $ instructions (iterateLSystem 1 sierpinski) @?= [Forward,TurnLeft,Forward,TurnRight,Forward,TurnRight,Forward,TurnLeft,Forward,TurnLeft,Forward,Forward,TurnLeft,Forward,Forward]
  ]

propertyChecks :: TestTree
propertyChecks = testGroup "Property tests (QuickCheck)"
  [ ]
  
