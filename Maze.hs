module Maze (
  Cell (Wall),
  Maze,
  isWalkable,
  parseMaze,
  printMaze
) where

import           Data.Char
import           Prelude   hiding (Left, Right)


data Direction = Up | Right | Down | Left

data Cell = Start
          | Path
          | Wall
          | Hole
          | Splitter
          | Function { name :: String }
          | Once
          | UnlessDetect { direction :: Direction }
          | Input
          | Output
          | Direction { direction :: Direction }
          | Pause { ticks :: Int }

type Maze = [[Cell]]


isWalkable :: Cell -> Bool
isWalkable Wall = False
isWalkable _    = True


parseMaze :: String -> Maze
parseMaze file = map parseRow (lines file)

parseRow :: String -> [Cell]
parseRow (a:b:cs) = case parseCell [a, b] of
                      Just cell -> cell:(parseRow cs)
                      Nothing   -> parseRow (b:cs)
parseRow _ = []

parseCell :: String -> Maybe Cell
parseCell "^^" = Just Start
parseCell ".." = Just Path
parseCell "##" = Just Wall
parseCell "()" = Just Hole
parseCell "<>" = Just Splitter
parseCell "--" = Just Once
parseCell "*U" = Just (UnlessDetect Up)
parseCell "*R" = Just (UnlessDetect Right)
parseCell "*D" = Just (UnlessDetect Down)
parseCell "*L" = Just (UnlessDetect Left)
parseCell "<<" = Just Input
parseCell ">>" = Just Output
parseCell "%U" = Just (Direction Up)
parseCell "%R" = Just (Direction Right)
parseCell "%D" = Just (Direction Down)
parseCell "%L" = Just (Direction Left)
parseCell ab@(a:b:"") | isAlpha a && isAlphaNum b = Just (Function ab)
                      | isNumber a && isNumber b  = Just (Pause (read ab))
                      | otherwise                 = Nothing

printMaze :: Maze -> IO ()
printMaze = mapM_ (\r -> do mapM_ printCell r
                            putStr "\n")


printCell :: Cell -> IO ()
printCell Start                              = putStr "^^ "
printCell Path                               = putStr ".. "
printCell Wall                               = putStr "## "
printCell Hole                               = putStr "() "
printCell Splitter                           = putStr "<> "
printCell Function { name = n }              = putStr (n ++ " ")
printCell Once                               = putStr "-- "
printCell UnlessDetect { direction = Up    } = putStr "*U "
printCell UnlessDetect { direction = Right } = putStr "*R "
printCell UnlessDetect { direction = Down  } = putStr "*D "
printCell UnlessDetect { direction = Left  } = putStr "*L "
printCell Input                              = putStr "<< "
printCell Output                             = putStr ">> "
printCell Direction { direction = Up    }    = putStr "%U "
printCell Direction { direction = Right }    = putStr "%R "
printCell Direction { direction = Down  }    = putStr "%D "
printCell Direction { direction = Left  }    = putStr "%L "
printCell Pause { ticks = t }                = putStr (show t ++ " ")
