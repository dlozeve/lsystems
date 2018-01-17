module Examples
  ( lsystems
    -- * Space-filling curves
  , gosper
  , hilbert
    -- * Fractals
  , levyC
  , koch
  , kochSnowflake
  , sierpinski
  , sierpinskiArrow
  , dragon
  , tree
  , plant
  -- * Tilings
  , penroseP3
  ) where

import Lib

-- | List of all exported L-systems
lsystems :: [LSystem Char]
lsystems = [gosper, hilbert, levyC, koch, kochSnowflake, sierpinski, sierpinskiArrow,
            dragon, tree, plant, penroseP3]

-- | Gosper curve
gosper :: LSystem Char
gosper =
  LSystem
  "gosper"
  "AB+-"
  "A"
  [ ('A', "A-B--B+A++AA+B-")
  , ('B', "+A-BB--B-A++A+B")]
  60
  10
  [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Hilbert curve
hilbert =
  LSystem
  "hilbert"
  "ABF+-"
  "A"
  [ ('A', "-BF+AFA+FB-")
  , ('B', "+AF-BFB-FA+")]
  90
  10
  [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Lévy C curve
levyC =
  LSystem
  "levyC"
  "F+-"
  "F"
  [('F', "+F--F+")]
  45
  10
  [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Koch curve
koch =
  LSystem
  "koch"
  "F+-"
  "F"
  [('F', "F+F-F-F+F")]
  90
  10
  [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Koch snowflake
kochSnowflake =
  LSystem
  "kochSnowflake"
  "F+-"
  "F"
  [('F', "F-F++F-F")]
  60
  10
  [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Sierpinski triangle
sierpinski =
  LSystem
  "sierpinski"
  "AB+-"
  "A-B-B"
  [ ('A', "A-B+A+B-A")
  , ('B', "BB")]
  120
  10
  [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Sierpinski arrowhead curve
sierpinskiArrow =
  LSystem
  "sierpinskiArrow"
  "AB+-"
  "A"
  [ ('A', "B+A+B")
  , ('B', "A-B-A")]
  60
  10
  [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Dragon curve
dragon =
  LSystem
  "dragon"
  "FX+-"
  "FX"
  [('X', "X+YF+"),
   ('Y', "-FX-Y")]
  90
  10
  [('F',Forward), ('+',TurnRight), ('-',TurnLeft)]

-- | Binary tree
tree =
  LSystem
  "tree"
  "AB+-[]"
  "A"
  [('B', "BB")
  ,('A', "B[+A]-A")]
  45
  1
  [('A',Forward), ('B',Forward), ('+',TurnRight), ('-',TurnLeft), ('[',Push), (']',Pop)]
  
-- | Fractal plant
plant =
  LSystem
  "plant"
  "FX+-[]"
  "X"
  [('X', "F[-X][X]F[-X]+FX")
  ,('F', "FF")]
  25
  1
  [('F',Forward), ('+',TurnRight), ('-',TurnLeft), ('[',Push), (']',Pop)]
  
-- | Penrose P3
penroseP3 =
  LSystem
  "penroseP3"
  "MNOPA+-[]"
  "[N]++[N]++[N]++[N]++[N]"
  [('M',"OA++PA----NA[-OA----MA]++")
  ,('N',"+OA--PA[---MA--NA]+")
  ,('O',"-MA++NA[+++OA++PA]-")
  ,('P',"--OA++++MA[+PA++++NA]--NA")
  ,('A',"")]
  36
  10
  [('M',Forward), ('N',Forward), ('O',Forward), ('P',Forward), ('A',Forward),
   ('+',TurnRight), ('-',TurnLeft), ('[',Push), (']',Pop)]
