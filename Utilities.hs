module Utilities (
  lpad,
  rpad,
  zpad
) where

lpad :: Char -> Int -> String -> String
lpad c n s = let npad = max (n - (length s)) 0 in take npad (repeat c) ++ s

rpad :: Char -> Int -> String -> String
rpad c n s = let npad = max (n - (length s)) 0 in s ++ take npad (repeat c)

zpad :: Int -> String -> String
zpad = lpad '0'
