module Examples
  ( gosper
  , hilbert
  , koch
  , sierpinski
  , sierpinskiArrow
  ) where

import Lib

gosper :: LSystem Char
gosper = LSystem
         "AB+-"
         "A"
         [ ('A', "A-B--B+A++AA+B-")
         , ('B', "+A-BB--B-A++A+B")]
         60
         10
         [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]

hilbert = LSystem
          "ABF+-"
          "A"
          [ ('A', "-BF+AFA+FB-")
          , ('B', "+AF-BFB-FA+")]
          90
          10
          [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

koch = LSystem
       "F+-"
       "F"
       [('F', "F+F-F-F+F")]
       90
       10
       [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

sierpinski = LSystem
             "AB+-"
             "A-B-B"
             [ ('A', "A-B+A+B-A")
             , ('B', "BB")]
             120
             10
             [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]

sierpinskiArrow = LSystem
                  "AB+-"
                  "A"
                  [ ('A', "B+A+B")
                  , ('B', "A-B-A")]
                  60
                  10
                  [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]
