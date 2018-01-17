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
  Nothing -> Left $ "Cannot find L-system \"" ++ s ++ "\". Use -l to find all available L-systems."

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

listLSystems :: Parser (a -> a)
listLSystems = infoOption (printList lsystems)
  (long "list-lsystems"
   <> short 'l'
   <> help "List all available L-systems")
  where printList xs = "Available L-systems:\n" ++ unlines (map name xs)

options :: Parser Options
options = Options <$> lsystem <*> iterations

opts :: ParserInfo Options
opts = info (options <**> listLSystems <**> helper)
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
