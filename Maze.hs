module Maze (
  Cell (..),
  Maze,
  isWalkable
) where

import           Utilities


data Cell = Start
          | Path
          | Wall
          | Hole
          | Splitter
          | Function { cFunctionName :: String }
          | Once
          | UnlessDetect { cDirection :: Direction }
          | Input
          | Output
          | Direction { cDirection :: Direction }
          | Pause { cTicks :: Int }
  deriving Show

type Maze = [[Cell]]


isWalkable :: Cell -> Bool
isWalkable Wall = False
isWalkable _    = True
