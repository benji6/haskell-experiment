module Compile (compile) where

import Data.Char (isLetter, isNumber, isSpace)
import Data.List (intersperse)

data Token = TIdentifier String
           | TNumber String
           | TParensLeft
           | TParensRight deriving (Show)

tokenizeLetters :: String -> [Token]
tokenizeLetters str = TIdentifier a : tokenize b
  where (a, b) = span isLetter str

tokenizeNumbers :: String -> [Token]
tokenizeNumbers str = TNumber a : tokenize b
  where (a, b) = span isNumber str

tokenize :: String -> [Token]
tokenize []         = []
tokenize ('(' : xs) = TParensLeft : tokenize xs
tokenize (')' : xs) = TParensRight : tokenize xs
tokenize (x : xs)
  | isSpace x = tokenize xs
  | isNumber x = tokenizeNumbers (x : xs)
  | isLetter x = tokenizeLetters (x : xs)
  | otherwise  = undefined

data Grammar = CallExpression [Grammar]
             | CommaHack
             | Identifier String
             | NumberLiteral String deriving (Show)

extractCallExpression :: [Token] -> Int -> [Token]
extractCallExpression [] _                              = []
extractCallExpression (TParensLeft : xs) bracketOffset  = TParensLeft : extractCallExpression xs (bracketOffset + 1)
extractCallExpression (TParensRight : xs) bracketOffset = if bracketOffset == 1
                                                          then []
                                                          else TParensRight : extractCallExpression xs (bracketOffset - 1)
extractCallExpression (x : xs) bracketOffset = x : extractCallExpression xs bracketOffset

parseCallExpression :: [Token] -> [Grammar]
parseCallExpression xs = [CallExpression $ parse $ callExpression] ++ parse (drop (length callExpression) xs)
  where callExpression = extractCallExpression xs 1

parse :: [Token] -> [Grammar]
parse []                   = []
parse (TParensLeft : xs)   = parseCallExpression xs
parse (TIdentifier x : xs) = Identifier x : parse xs
parse (TNumber x : xs)     = NumberLiteral x : parse xs
parse (_ : xs)             = parse xs

generateCallee :: Grammar -> String
generateCallee (Identifier a) = a ++ "("

generate :: [Grammar] -> String
generate []                         = []
generate (CallExpression args : xs) = concat [
    generateCallee $ head args,
    generate (intersperse CommaHack (tail args)),
    ")",
    generate xs
  ]
generate (CommaHack : xs)           = ", " ++ generate xs
generate (Identifier x : xs)        = x ++ generate xs
generate (NumberLiteral x : xs)     = x ++ generate xs

compile = generate . parse . tokenize
