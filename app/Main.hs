module Main where

import Graphics.Gloss
import Options.Applicative
import Data.Semigroup ((<>))
import Data.List
import Safe

import Lib
import Examples

data Options = Options
  { optionLSystem :: LSystem Char
  , optionIterations :: Integer
  , optionColor :: Color
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

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
                "" -> []
                s' -> w:(splitOn c s'')
                  where (w, s'') = break (== c) s'

colorParser :: Parser Color
colorParser = option (eitherReader readRGB)
  (long "color"
   <> short 'c'
   <> help "Foreground color RGBA (0-255)"
   <> showDefault
   <> value white
   <> metavar "R,G,B")
  where readRGB  s = do
          case mapM readEitherSafe $ splitOn ',' s of
            Right (r:g:b:a:_) -> Right $ makeColorI r g b a
            Right (r:g:b:_) -> Right $ makeColorI r g b 255
            Right (r:g:_) -> Right $ makeColorI r g 255 255
            Right (r:_) -> Right $ makeColorI r 255 255 255
            Right  _  -> Right $ makeColorI 255 255 255 255
            Left s -> Left s

options :: Parser Options
options = Options <$> lsystem <*> iterations <*> colorParser

opts :: ParserInfo Options
opts = info (options <**> listLSystems <**> helper)
  ( fullDesc
  <> progDesc "Generate and draw an L-system"
  <> header "lsystems -- Generate L-systems")

createDisplay :: (Eq a, Integral p) => Color -> p -> LSystem a -> IO ()
createDisplay c n ls = display (InWindow "L-System" (200, 200) (10, 10)) black (color c pic)
  where pic = drawLSystem $ iterateLSystem n ls

main :: IO ()
main = do
  Options ls n c <- execParser opts
  createDisplay c n ls
