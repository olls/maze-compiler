module Main (main) where

import           System.Environment (getArgs)

import           FunctionParser
import           MazeParser
import           TokenParser


main :: IO ()
main = do
  args <- getArgs
  let filename = args!!0
  putStr ("Compiling maze: " ++ filename ++ "\n")
  file <- readFile filename

  let tokens = parseTokens file
  let (tokens', functions) = parseFunctions tokens
  let maze = parseMaze tokens'

  printMaze maze
  putChar '\n'
  mapM_ print functions
