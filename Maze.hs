module Maze (
  Maze,
  Cell (..),
  FunctionDef (..),
  Direction (..),
  isWalkable,
  printMaze
) where

import           Prelude   hiding (Left, Right)


data Direction = Up | Right | Down | Left
  deriving Show

type Maze = ([[Cell]], [FunctionDef])

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

data FunctionDef = FunctionDef {
  fdName :: String
} deriving Show


isWalkable :: Cell -> Bool
isWalkable Wall = False
isWalkable _    = True


printMaze :: [[Cell]] -> IO ()
printMaze = mapM_ (\r -> do mapM_ printCell r
                            putStr "\n")


printCell :: Cell -> IO ()
printCell Start                               = putStr "^^ "
printCell Path                                = putStr ".. "
printCell Wall                                = putStr "## "
printCell Hole                                = putStr "() "
printCell Splitter                            = putStr "<> "
printCell Function { cFunctionName = n }      = putStr (n ++ " ")
printCell Once                                = putStr "-- "
printCell UnlessDetect { cDirection = Up    } = putStr "*U "
printCell UnlessDetect { cDirection = Right } = putStr "*R "
printCell UnlessDetect { cDirection = Down  } = putStr "*D "
printCell UnlessDetect { cDirection = Left  } = putStr "*L "
printCell Input                               = putStr "<< "
printCell Output                              = putStr ">> "
printCell Direction { cDirection = Up    }    = putStr "%U "
printCell Direction { cDirection = Right }    = putStr "%R "
printCell Direction { cDirection = Down  }    = putStr "%D "
printCell Direction { cDirection = Left  }    = putStr "%L "
printCell Pause { cTicks = t }                = putStr (show t ++ " ")
