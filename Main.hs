module Main (main) where

import           System.Environment (getArgs)

import           Maze


main :: IO ()
main = do
  args <- getArgs
  let filename = args!!0
  putStr ("Compiling maze: " ++ filename ++ "\n")
  file <- readFile filename
  let maze = parseMaze file
  printMaze (fst maze)
