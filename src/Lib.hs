module Lib
    ( -- * L-system data types
      LSystem(..)
    , Instruction(..)
      -- * L-system functions
    , iterateLSystem
    , instructions
    , drawLSystem
    ) where

import Data.Maybe
import Graphics.Gloss

-- | L-system data type
data LSystem a = LSystem
  { alphabet :: [a] -- ^ variables and constants used by the system
  , axiom :: [a] -- ^ initial state of the system
  , rules :: [(a, [a])] -- ^ production rules defining how each
                        -- variable can be replaced by a sequence of
                        -- variables and constants
  , angle :: Float -- ^ angle used for the representation
  , distance :: Float -- ^ distance of each segment in the representation
  , representation :: [(a, Instruction)] -- ^ representation rules
                                         -- defining how each variable
                                         -- and constant should be
                                         -- represented
  } deriving (Eq, Show)

-- | Instructions for displaying the L-system
data Instruction =
  Forward -- ^ move forward
  | TurnRight -- ^ turn right by angle
  | TurnLeft -- ^ turn left by angle
  | Stay -- ^ do nothing
  deriving (Eq, Show)


-- | Iterate the L-system by n steps
iterateLSystem :: (Eq a, Integral t) => t -> LSystem a -> LSystem a
iterateLSystem 0 lsystem = lsystem
iterateLSystem n (LSystem a ax r ang dist rep) =
  iterateLSystem (n-1) $ LSystem a ax' r ang dist rep
  where ax' = concat $ map f ax
        f x = case lookup x r of
                Just xs -> xs
                Nothing -> [x]

-- | Generate a set of instructions from an L-system
instructions :: Eq a => LSystem a -> [Instruction]
instructions (LSystem a ax r ang dist rep) = mapMaybe f ax
  where f x = lookup x rep

-- | Draw a sequence of instructions
turtle :: Float -- ^ angle
  -> Float -- ^ distance
  -> [Instruction] -- ^ sequence of instruction
  -> Picture -- ^ generated picture
turtle angle distance = go 0 (Line [(0,0)])
  where go _ ps [] = ps
        go theta (Line path) (x:xs) =
          case x of
            Forward -> go theta (Line (p:path)) xs
            TurnRight -> go (theta + angle) (Line path) xs
            TurnLeft -> go (theta - angle) (Line path) xs
            Stay -> go theta (Line path) xs
          where
            (px, py) = head path
            thetaRad = theta * pi / 180
            p = (px + distance * cos thetaRad, py + distance * sin thetaRad)

-- | Draw an L-system
drawLSystem :: Eq a => LSystem a -> Picture
drawLSystem ls@(LSystem a ax r ang dist rep) = turtle ang dist $ instructions ls

