module Utilities (
  lpad,
  rpad,
  zpad,
  Direction (..),
  direction
) where

import           Prelude     hiding (Left, Right)


lpad :: Char -> Int -> String -> String
lpad c n s = let npad = max (n - (length s)) 0 in take npad (repeat c) ++ s

rpad :: Char -> Int -> String -> String
rpad c n s = let npad = max (n - (length s)) 0 in s ++ take npad (repeat c)

zpad :: Int -> String -> String
zpad = lpad '0'


data Direction = Up | Right | Down | Left
  deriving Show

direction :: String -> Direction
direction (_:d:"") | d == 'U' || d == 'u' = Up
direction (_:d:"") | d == 'R' || d == 'r' = Right
direction (_:d:"") | d == 'D' || d == 'd' = Down
direction (_:d:"") | d == 'L' || d == 'l' = Left
