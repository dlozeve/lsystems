{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Gloss
import Options.Applicative
import Data.Semigroup ((<>))
import Data.List
import Safe

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

import Lib

data CmdLnOptions = CmdLnOptions
  { optionIterations :: Integer
  , optionColor :: Color
  , optionWhiteBg :: Bool
  }

iterationsParser :: Parser Integer
iterationsParser = option auto
  (long "iterations"
   <> short 'n'
   <> help "Number of iterations"
   <> showDefault
   <> value 5
   <> metavar "N")

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : splitOn c s''
                  where (w, s'') = break (== c) s'

colorParser :: Parser Color
colorParser = option (eitherReader readRGB)
  (long "color"
   <> short 'c'
   <> help "Foreground color RGBA (0-255)"
   <> showDefault
   <> value white
   <> metavar "R,G,B")
  where readRGB  s =
          case mapM readEitherSafe $ splitOn ',' s of
            Right (r:g:b:a:_) -> Right $ makeColorI r g b a
            Right (r:g:b:_) -> Right $ makeColorI r g b 255
            Right (r:g:_) -> Right $ makeColorI r g 255 255
            Right (r:_) -> Right $ makeColorI r 255 255 255
            Right  _  -> Right $ makeColorI 255 255 255 255
            Left s -> Left s

whiteBackgroundParser :: Parser Bool
whiteBackgroundParser = switch
  (long "white-background"
   <> short 'w'
   <> help "Use a white background")

optionsParser :: Parser CmdLnOptions
optionsParser = CmdLnOptions <$>
  iterationsParser <*> colorParser <*> whiteBackgroundParser

opts :: ParserInfo CmdLnOptions
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Generate and draw an L-system"
  <> header "lsystems -- Generate L-systems")

createDisplay :: (Eq a, Integral p) => Color -> Bool -> p -> LSystem a -> IO ()
createDisplay fgColor wbg n ls = display (InWindow (name ls) (200, 200) (10, 10)) bgColor (color fgColor pic)
  where pic = drawLSystem $ iterateLSystem n ls
        bgColor = if wbg then white else black

main :: IO ()
main = do
  lsStr <- B.getContents
  let Just ls = decode lsStr :: Maybe (LSystem Char)
  CmdLnOptions n fgColor wbg <- execParser opts
  createDisplay fgColor wbg n ls
