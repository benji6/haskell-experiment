module Main (main) where

import System.Environment (getArgs)
import Compile (compile)

interactWith f inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile $ f input

main = do
  args <- getArgs
  case args of
    [input, output] -> interactWith compile input output
    _ -> putStrLn "error: too many arguments"
