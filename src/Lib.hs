{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( -- * L-system data types
      LSystem(..)
    , Instruction(..)
      -- * L-system functions
    , iterateLSystem
    , instructions
    , turtle
    , drawLSystem
    ) where

import Data.Maybe
import Graphics.Gloss
import GHC.Generics
import Data.Aeson
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.ByteString.Lazy as B

-- | L-system data type
data LSystem a = LSystem
  { name :: String
  , alphabet :: [a] -- ^ variables and constants used by the system
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
  } deriving (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (LSystem a)

instance (ToJSON a) => ToJSON (LSystem a) where
    toEncoding = genericToEncoding defaultOptions

-- | Instructions for displaying the L-system
data Instruction =
  Forward -- ^ move forward
  | TurnRight -- ^ turn right by angle
  | TurnLeft -- ^ turn left by angle
  | Push -- ^ push a position on the stack
  | Pop -- ^ pop a position from the stack
  | Stay -- ^ do nothing
  deriving (Eq, Show, Generic)

instance FromJSON Instruction where
  parseJSON = withText "Instruction" $ \s ->
    if s `elem` ["Forward", "forward", "F", "f"] then
      pure Forward
    else if s `elem` ["TurnRight", "Turnright", "turnright", "Right", "right", "R", "r"] then
      pure TurnRight
    else if s `elem` ["TurnLeft", "Turnleft", "turnleft", "Left", "left", "L", "l"] then
      pure TurnLeft
    else if s `elem` ["Push", "push"] then
      pure Push
    else if s `elem` ["Pop", "pop"] then
      pure Pop
    else
      pure Stay

instance ToJSON Instruction where
  toEncoding = genericToEncoding defaultOptions

-- | Iterate the L-system by n steps
iterateLSystem :: (Eq a, Integral t) => t -> LSystem a -> LSystem a
iterateLSystem 0 lsystem = lsystem
iterateLSystem n lsystem | n < 0 = iterateLSystem (-n) lsystem
iterateLSystem n (LSystem na a ax r ang dist rep) =
  iterateLSystem (n-1) $ LSystem na a ax' r ang dist rep
  where ax' = concatMap f ax
        f x = fromMaybe [x] (lookup x r)

-- | Generate a set of instructions from an L-system
instructions :: Eq a => LSystem a -> [Instruction]
instructions (LSystem na a ax r ang dist rep) = mapMaybe f ax
  where f x = lookup x rep

-- | Draw a sequence of instructions
turtle :: Float -- ^ angle
  -> Float -- ^ distance
  -> [Instruction] -- ^ sequence of instruction
  -> Picture -- ^ generated picture
turtle angle distance = go 90 (Line [(0,0)]) (Pictures []) []
  where
    go :: Float -> Picture -> Picture -> [(Point,Float)] -> [Instruction] -> Picture
    go _ line (Pictures ps) _ [] = Pictures (line:ps)
    go theta (Line path) (Pictures ps) stack (x:xs) =
      case x of
        Forward -> go theta (Line (p:path)) (Pictures ps) stack xs
        TurnRight -> go (theta + angle) (Line path) (Pictures ps) stack xs
        TurnLeft -> go (theta - angle) (Line path) (Pictures ps) stack xs
        Push -> go theta (Line path) (Pictures ps) ((head path, theta):stack) xs
        Pop -> let (pos, theta'):t = stack in
          go theta' (Line [pos]) (Pictures (Line path : ps)) t xs
        Stay -> go theta (Line path) (Pictures ps) stack xs
      where
        (px, py) = head path
        thetaRad = theta * pi / 180
        p = (px + distance * cos thetaRad, py + distance * sin thetaRad)

-- | Draw an L-system
drawLSystem :: Eq a => LSystem a -> Picture
drawLSystem ls@(LSystem na a ax r ang dist rep) = turtle ang dist $ instructions ls

