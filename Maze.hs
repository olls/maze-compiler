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
  deriving Show

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

type Maze = ([[Cell]], [FunctionDef])

data FunctionDef = FunctionDef {
  fdName :: String
} deriving Show


isWalkable :: Cell -> Bool
isWalkable Wall = False
isWalkable _    = True


parseMaze :: String -> Maze
parseMaze file = foldr collapse ([[]], []) (map parseRow (lines file))
  where
    collapse :: ([Cell], [FunctionDef]) -> Maze -> Maze
    collapse (row, fds') (maze, fds) = ((row:maze), (fds'++fds))

    parseRow :: String -> ([Cell], [FunctionDef])
    parseRow (a:b:row) =
      case parseCell (a, b) of
        Nothing   -> let (cs, fds) = parseRow (b:row) in (cs, fds)
        Just cell ->
          case cell of
            Function {} ->
              case getArrow row of
                Nothing             -> let (cs, fds) = parseRow row in (cell:cs, fds)
                Just functionString ->
                  case parseFunctionDef functionString of
                    Nothing -> ([], [])
                    Just fd -> ([], [fd])
            otherwise   -> let (cs, fds) = parseRow row in (cell:cs, fds)
    parseRow _ = ([], [])

parseCell :: (Char, Char) -> Maybe Cell
parseCell ('^','^') = Just Start
parseCell ('.','.') = Just Path
parseCell ('#','#') = Just Wall
parseCell ('(',')') = Just Hole
parseCell ('<','>') = Just Splitter
parseCell ('-','-') = Just Once
parseCell ('*','U') = Just (UnlessDetect Up)
parseCell ('*','R') = Just (UnlessDetect Right)
parseCell ('*','D') = Just (UnlessDetect Down)
parseCell ('*','L') = Just (UnlessDetect Left)
parseCell ('<','<') = Just Input
parseCell ('>','>') = Just Output
parseCell ('%','U') = Just (Direction Up)
parseCell ('%','R') = Just (Direction Right)
parseCell ('%','D') = Just (Direction Down)
parseCell ('%','L') = Just (Direction Left)
parseCell (a, b) | isFunction a b           = Just (Function [a, b])
                 | isNumber a && isNumber b = Just (Pause (read [a, b]))
                 | otherwise                = Nothing


isFunction :: Char -> Char -> Bool
isFunction a b = isAlpha a && isAlphaNum b

getArrow :: String -> Maybe String
getArrow ('-':'>':rest) = Just rest
getArrow (_:rest)       = getArrow rest
getArrow []             = Nothing

parseFunctionDef :: String -> Maybe FunctionDef
parseFunctionDef s = Just (FunctionDef { fdName = "AA" })


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
