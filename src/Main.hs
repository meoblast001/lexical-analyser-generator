{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Main where

import Rules
import StartEndTable
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
  either (putStrLn . show) processRules (parse contents)

processRules :: [Rule] -> IO ()
processRules rules = do
  putStrLn "Rules:"
  mapM_ (putStrLn . show) rules
  putStrLn ""
  mapM_ printRegexTable rules

printRegexTable :: Rule -> IO ()
printRegexTable rule@(Token name rx) = do
  putStrLn ("Start-end table for \"" ++ name ++ "\":")
  putStrLn $ (show $ buildStartEndTable rx)
printRegexTable ignore@(Ignore rx) = do
  putStrLn "Start-end table for ignore:"
  putStrLn $ (show $ buildStartEndTable rx)
printRegexTable _ = do return ()
