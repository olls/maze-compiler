module TokenParser (
  Token (..),
  parseTokens
) where

import           Data.Char
import           Data.List


data Token = Start
           | Path
           | Wall
           | Hole
           | Splitter
           | Function     { src :: String }
           | Once
           | UnlessDetect { src :: String }
           | Input
           | Output
           | Direction    { src :: String }
           | Number       { src :: String }
           | Arrow
           | If
           | Then
           | Else
           | Equals
           | GreaterEqual
           | Greater
           | LessEqual
           | Less
           | Increment
           | Decrement
           | Multiply
           | Divide
           | NewLine
  deriving Show


parseTokens :: String -> [Token]
parseTokens s | isPrefixOf "^^"    s =        Start            : parseTokens (drop 2 s)
              | isPrefixOf ".."    s =         Path            : parseTokens (drop 2 s)
              | isPrefixOf "##"    s =         Wall            : parseTokens (drop 2 s)
              | isPrefixOf "()"    s =         Hole            : parseTokens (drop 2 s)
              | isPrefixOf "<>"    s =     Splitter            : parseTokens (drop 2 s)
              | isPrefixOf "--"    s =         Once            : parseTokens (drop 2 s)
              | isPrefixOf "*U"   us = UnlessDetect (take 2 s) : parseTokens (drop 2 s)
              | isPrefixOf "*R"   us = UnlessDetect (take 2 s) : parseTokens (drop 2 s)
              | isPrefixOf "*D"   us = UnlessDetect (take 2 s) : parseTokens (drop 2 s)
              | isPrefixOf "*L"   us = UnlessDetect (take 2 s) : parseTokens (drop 2 s)
              | isPrefixOf "<<"    s =        Input            : parseTokens (drop 2 s)
              | isPrefixOf ">>"    s =       Output            : parseTokens (drop 2 s)
              | isPrefixOf "%U"   us =    Direction (take 2 s) : parseTokens (drop 2 s)
              | isPrefixOf "%R"   us =    Direction (take 2 s) : parseTokens (drop 2 s)
              | isPrefixOf "%D"   us =    Direction (take 2 s) : parseTokens (drop 2 s)
              | isPrefixOf "%L"   us =    Direction (take 2 s) : parseTokens (drop 2 s)
              | isPrefixOf "->"    s =        Arrow            : parseTokens (drop 2 s)
              | isPrefixOf "IF"   us =           If            : parseTokens (drop 2 s)
              | isPrefixOf "THEN" us =         Then            : parseTokens (drop 4 s)
              | isPrefixOf "ELSE" us =         Else            : parseTokens (drop 4 s)
              | isPrefixOf "="     s =       Equals            : parseTokens (drop 1 s)
              | isPrefixOf ">="    s = GreaterEqual            : parseTokens (drop 2 s)
              | isPrefixOf ">"     s =      Greater            : parseTokens (drop 1 s)
              | isPrefixOf "<="    s =    LessEqual            : parseTokens (drop 2 s)
              | isPrefixOf "<"     s =         Less            : parseTokens (drop 1 s)
              | isPrefixOf "+="    s =    Increment            : parseTokens (drop 2 s)
              | isPrefixOf "-="    s =    Decrement            : parseTokens (drop 2 s)
              | isPrefixOf "*="    s =     Multiply            : parseTokens (drop 2 s)
              | isPrefixOf "/="    s =       Divide            : parseTokens (drop 2 s)
              | length s >= 2 && isAlpha (s!!0) && isAlphaNum (s!!1) =
                                           Function (take 2 s) : parseTokens (drop 2 s)
  where us = map toUpper s

parseTokens (c:rest) | isNumber c = Number [c] : parseTokens rest
parseTokens ('\n':rest) = NewLine : parseTokens rest

parseTokens (_:rest) = parseTokens rest
parseTokens "" = []
