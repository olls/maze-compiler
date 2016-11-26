module FunctionParser (
  FunctionDef (..),
  parseFunctions
) where

import Text.Show.Functions ()

import           TokenParser (Token)
import qualified TokenParser as Tkn
import Car
import Utilities


type FunctionCondition = Car -> Bool

data FunctionBody = FunctionMaths     { ff :: Integer -> Integer }
                  | FunctionIf        { fifCond :: FunctionCondition,
                                        fifThen :: Direction,
                                        fifElse :: Maybe Direction }
  deriving Show


data FunctionDef = FunctionDef {
  fdName :: String,
  fdBody :: FunctionBody
} deriving Show


parseFunctions :: [Token] -> ([Token], [FunctionDef])
parseFunctions (Tkn.Function fName : Tkn.Arrow : ts) = (ts'', functionDef ++ fds)
  where (ts', functionBody) = parseFunctionInner ts
        functionDef = case functionBody of
          Just fb -> [FunctionDef { fdName = fName, fdBody = fb}]
          Nothing -> []
        (ts'', fds) = parseFunctions ts'
parseFunctions (t:ts) = (t:ts', fds)
  where (ts', fds) = parseFunctions ts

parseFunctions [] = ([], [])


parseFunctionInner :: [Token] -> ([Token], Maybe FunctionBody)
parseFunctionInner    (Tkn.Equals : Tkn.Number iN : ts) = (ts', Just FunctionMaths { ff = \_ -> n })
  where (ts', n) = restOfNumber iN ts
parseFunctionInner (Tkn.Increment : Tkn.Number iN : ts) = (ts', Just FunctionMaths { ff = (+) n })
  where (ts', n) = restOfNumber iN ts
parseFunctionInner (Tkn.Decrement : Tkn.Number iN : ts) = (ts', Just FunctionMaths { ff = (-) n })
  where (ts', n) = restOfNumber iN ts
parseFunctionInner  (Tkn.Multiply : Tkn.Number iN : ts) = (ts', Just FunctionMaths { ff = (*) n })
  where (ts', n) = restOfNumber iN ts
parseFunctionInner    (Tkn.Divide : Tkn.Number iN : ts) = (ts', Just FunctionMaths { ff = div n })
  where (ts', n) = restOfNumber iN ts

parseFunctionInner (Tkn.If : ts) =
  case mfCond of
    Just fCond -> case ts' of
      (Tkn.Then : Tkn.Direction d : ts'') -> let (ts''', elseD) = parseIfElse ts'' in
                                               (ts''', Just FunctionIf { fifCond = fCond, fifThen = (direction d), fifElse = elseD })
      otherwise -> (ts', Nothing)
    Nothing -> (ts', Nothing)
  where (ts', mfCond) = parseIfCond ts

parseFunctionInner (_ : ts) = (ts, Nothing)


parseIfCond :: [Token] -> ([Token], Maybe FunctionCondition)
parseIfCond (Tkn.GreaterEqual : Tkn.Number iN : ts) = (ts', Just (\Car {cValue = value} -> value >= n))
  where (ts', n) = restOfNumber iN ts
parseIfCond (Tkn.Greater      : Tkn.Number iN : ts) = (ts', Just (\Car {cValue = value} -> value > n))
  where (ts', n) = restOfNumber iN ts
parseIfCond (Tkn.LessEqual    : Tkn.Number iN : ts) = (ts', Just (\Car {cValue = value} -> value <= n))
  where (ts', n) = restOfNumber iN ts
parseIfCond (Tkn.Less         : Tkn.Number iN : ts) = (ts', Just (\Car {cValue = value} -> value < n))
  where (ts', n) = restOfNumber iN ts
parseIfCond ts = (ts, Nothing)

parseIfElse :: [Token] -> ([Token], Maybe Direction)
parseIfElse (Tkn.Else : Tkn.Direction d : ts) = (ts, Just (direction d))
parseIfElse ts = (ts, Nothing)


restOfNumber :: String -> [Token] -> ([Token], Integer)
restOfNumber iN ts = let (ts', endN) = getDigit ts in (ts', read (iN ++ endN))
  where
    getDigit (Tkn.Number n : ts) = (ts', n ++ ns)
      where (ts', ns) = getDigit ts
    getDigit ts = (ts, "")
