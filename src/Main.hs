{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Main where

import Rules
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> process input
    _ -> error "Please provide an input file name."

process :: FilePath -> IO ()
process filename = do
  fileHandle <- openFile filename ReadMode
  contents <- hGetContents fileHandle
  putStrLn $ either id show (parse contents)
