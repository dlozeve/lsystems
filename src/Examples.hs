module Examples
  ( -- * Space-filling curves
    gosper
  , hilbert
    -- * Fractals
  , koch
  , kochSnowflake
  , sierpinski
  , sierpinskiArrow
  , dragon
  ) where

import Lib

-- | Gosper curve
gosper :: LSystem Char
gosper = LSystem
         "AB+-"
         "A"
         [ ('A', "A-B--B+A++AA+B-")
         , ('B', "+A-BB--B-A++A+B")]
         60
         10
         [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Hilbert curve
hilbert = LSystem
          "ABF+-"
          "A"
          [ ('A', "-BF+AFA+FB-")
          , ('B', "+AF-BFB-FA+")]
          90
          10
          [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Koch curve
koch = LSystem
       "F+-"
       "F"
       [('F', "F+F-F-F+F")]
       90
       10
       [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Koch snowflake
kochSnowflake = LSystem
                "F+-"
                "F"
                [('F', "F+F--F+F")]
                60
                10
                [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Sierpinski triangle
sierpinski = LSystem
             "AB+-"
             "A-B-B"
             [ ('A', "A-B+A+B-A")
             , ('B', "BB")]
             120
             10
             [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Sierpinski arrowhead curve
sierpinskiArrow = LSystem
                  "AB+-"
                  "A"
                  [ ('A', "B+A+B")
                  , ('B', "A-B-A")]
                  60
                  10
                  [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Dragon curve
dragon = LSystem
         "FX+-"
         "FX"
         [('X', "X+YF+"),
          ('Y', "-FX-Y")]
         90
         10
         [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]
