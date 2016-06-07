module Main (main) where

import Data.Char (isLetter, isNumber)
import Data.List (intersperse)

sourceCode = "(map (comp double (add 12) (sub 1) xs)"
expectedCode = "map(comp(double, add(12), sub(1), xs))"

data Token = TIdentifier String
           | TNumber String
           | TParensLeft
           | TParensRight deriving (Show)

splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen f xs = (takeWhile f xs, dropWhile f xs)

tokenizeLetters :: String -> [Token]
tokenizeLetters str = TIdentifier a : tokenize b
  where (a, b) = splitWhen isLetter str

tokenizeNumbers :: String -> [Token]
tokenizeNumbers str = TNumber a : tokenize b
  where (a, b) = splitWhen isNumber str

tokenize :: String -> [Token]
tokenize []         = []
tokenize (' ' : xs) = tokenize xs
tokenize ('(' : xs) = TParensLeft : tokenize xs
tokenize (')' : xs) = TParensRight : tokenize xs
tokenize (x : xs)
  | isNumber x = tokenizeNumbers (x : xs)
  | isLetter x = tokenizeLetters (x : xs)
  | otherwise  = undefined

data Grammar = CallExpression [Grammar]
             | Identifier String
             | NumberLiteral String deriving (Show)

parse :: [Token] -> [Grammar]
parse []                   = []
parse (TParensLeft : xs)   = [CallExpression (parse xs)]
parse (TIdentifier x : xs) = Identifier x : parse xs
parse (TNumber x : xs)     = NumberLiteral x : parse xs
parse (_ : xs)             = parse xs

generateArgs :: [Grammar] -> String
generateArgs (Identifier x : xs)    = concat [x, ", ", generateArgs xs]
generateArgs (NumberLiteral x : xs) = concat [x, ", ", generateArgs xs]
generateArgs x = generate x

generateCallee :: Grammar -> String
generateCallee (Identifier a) = a ++ "("

generate :: [Grammar] -> String
generate []                         = []
generate (CallExpression args : xs) = concat [
    generateCallee $ head args,
    generateArgs $ tail args,
    ")",
    generate xs
  ]
generate (Identifier x : xs)        = x ++ generate xs
generate (NumberLiteral x : xs)     = x ++ generate xs

compile = generate . parse . tokenize

main = do
  putStrLn $ show $ tokenize sourceCode
  putStrLn $ show $ parse $ tokenize sourceCode
  putStrLn $ show $ compile sourceCode
  putStrLn $ show $
    if (compile sourceCode == expectedCode)
    then "tests pass"
    else "tests fail"