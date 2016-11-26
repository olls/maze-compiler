module MazeParser (
  direction,
  isWalkable,
  parseMaze,
  printMaze
) where

import           Prelude     hiding (Left, Right)

import Maze
import           TokenParser (Token)
import qualified TokenParser as Tkn
import           Utilities


parseMaze :: [Token] -> Maze
parseMaze [] = []
parseMaze ts =  row : parseMaze ts'
  where (ts', row) = parseMazeRow ts


parseMazeRow :: [Token] -> ([Token], [Cell])
parseMazeRow (Tkn.NewLine:ts) = (ts, [])
parseMazeRow [] = ([], [])
parseMazeRow ts = case mCell of
                    Nothing   -> (ts'', cs)
                    Just cell -> (ts'', cell:cs)
  where
    (ts', mCell) = case ts of
      Tkn.Start                   : rest -> (rest, Just Start)
      Tkn.Path                    : rest -> (rest, Just Path)
      Tkn.Wall                    : rest -> (rest, Just Wall)
      Tkn.Hole                    : rest -> (rest, Just Hole)
      Tkn.Splitter                : rest -> (rest, Just Splitter)
      Tkn.Function           name : rest -> (rest, Just (Function name))
      Tkn.Once                    : rest -> (rest, Just Once)
      Tkn.UnlessDetect          d : rest -> (rest, Just (UnlessDetect (direction d)))
      Tkn.Input                   : rest -> (rest, Just Input)
      Tkn.Output                  : rest -> (rest, Just Output)
      Tkn.Direction             d : rest -> (rest, Just (Direction (direction d)))
      Tkn.Number a : Tkn.Number b : rest -> (rest, Just (Pause (read (a++b))))
      _ : rest                           -> (rest, Nothing)
    (ts'', cs) = parseMazeRow ts'


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
printCell Pause { cTicks = t }                = putStr (((zpad 2) . show) t ++ " ")
