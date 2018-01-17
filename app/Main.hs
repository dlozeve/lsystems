module Main where

import Graphics.Gloss
import Options.Applicative
import Data.Semigroup ((<>))
import Data.List

import Lib
import Examples

data Options = Options
  { optionLSystem :: LSystem Char
  , optionIterations :: Integer
  }

selectLSystem :: [LSystem a] -> String -> Either String (LSystem a)
selectLSystem ls s = case find (\x -> name x == s) ls of
  Just x -> Right x
  Nothing -> Left $ "Cannot find L-system \"" ++ s ++ "\""

lsystem :: Parser (LSystem Char)
lsystem = argument (eitherReader (selectLSystem lsystems))
  (metavar "LSYSTEM"
   <> help "L-system to generate"
   <> showDefaultWith name
   <> value penroseP3
   <> completeWith (map name lsystems)
   <> completer (listCompleter (map name lsystems)))

iterations :: Parser Integer
iterations = option auto
  (long "iterations"
   <> short 'n'
   <> help "Number of iterations"
   <> showDefault
   <> value 5
   <> metavar "N")

options :: Parser Options
options = Options <$> lsystem <*> iterations

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Generate and draw an L-system"
  <> header "lsystems -- Generate L-systems")

createDisplay :: (Eq a, Integral p) => p -> LSystem a -> IO ()
createDisplay n ls = display (InWindow "L-System" (200, 200) (10, 10)) black (color white pic)
  where pic = drawLSystem $ iterateLSystem n ls

main :: IO ()
main = do
  Options ls n <- execParser opts
  createDisplay n ls
