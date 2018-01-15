module Lib
    ( LSystem(..)
    , Instruction(..)
    , iterateLSystem
    , drawLSystem
    ) where

import Data.Maybe
import Graphics.Gloss

data LSystem a = LSystem
  { alphabet :: [a]
  , axiom :: [a]
  , rules :: [(a, [a])]
  , angle :: Float
  , distance :: Float
  , representation :: [(a, Instruction)]
  } deriving (Eq, Show)

data Instruction = Forward | TurnRight | TurnLeft | Stay
  deriving (Eq, Show)


iterateLSystem :: (Eq a, Num t, Eq t) => t -> LSystem a -> LSystem a
iterateLSystem 0 lsystem = lsystem
iterateLSystem n (LSystem a ax r ang dist rep) =
  iterateLSystem (n-1) $ LSystem a ax' r ang dist rep
  where ax' = concat $ map f ax
        f x = case lookup x r of
                Just xs -> xs
                Nothing -> [x]

instructions :: Eq a => LSystem a -> [Instruction]
instructions (LSystem a ax r ang dist rep) = mapMaybe f ax
  where f x = lookup x rep

turtle :: Float -> Float -> [Instruction] -> Picture
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

drawLSystem :: Eq a => LSystem a -> Picture
drawLSystem ls@(LSystem a ax r ang dist rep) = turtle ang dist $ instructions ls

