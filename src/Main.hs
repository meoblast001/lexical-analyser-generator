{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Main where

import FollowTable
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
  let seTable = buildStartEndTable rx
      followTable = buildFollowTable seTable
  putStrLn ("Start-end table for \"" ++ name ++ "\":")
  putStrLn $ show seTable
  putStrLn ("Follow table for \"" ++ name ++ "\":")
  putStrLn $ show followTable
printRegexTable ignore@(Ignore rx) = do
  let seTable = buildStartEndTable rx
      followTable = buildFollowTable seTable
  putStrLn "Start-end table for ignore:"
  putStrLn $ show seTable
  putStrLn "Follow table for ignore:"
  putStrLn $ show followTable
printRegexTable _ = do return ()
