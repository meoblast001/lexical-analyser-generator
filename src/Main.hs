{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Main where

import FollowTable
import Rules
import StartEndTable
import StateTable
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["info", input] -> displayInfo input
    _ -> error "Please provide an input file name."

displayInfo :: FilePath -> IO ()
displayInfo filename = do
  fileHandle <- openFile filename ReadMode
  contents <- hGetContents fileHandle
  either (putStrLn . show) displayRulesInfo (parse contents)

displayRulesInfo :: [Rule] -> IO ()
displayRulesInfo rules = do
  putStrLn "Rules:"
  mapM_ (putStrLn . show) rules
  putStrLn ""
  mapM_ printRegexTables rules

printRegexTables :: Rule -> IO ()
printRegexTables rule@(Token name rx) = do
  let seTable = buildStartEndTable rx
      followTable = buildFollowTable seTable
      incompStates = buildIncompEntries (head $ seTableEntries seTable) followTable
  putStrLn ("Start-end table for \"" ++ name ++ "\":")
  putStrLn $ show seTable
  putStrLn ("Follow table for \"" ++ name ++ "\":")
  putStrLn $ show followTable
  putStrLn ("Incomplete state table for \"" ++ name ++ "\":")
  mapM_ (putStrLn . show) incompStates >> putStr "\n"
printRegexTables ignore@(Ignore rx) = do
  let seTable = buildStartEndTable rx
      followTable = buildFollowTable seTable
  putStrLn "Start-end table for ignore:"
  putStrLn $ show seTable
  putStrLn "Follow table for ignore:"
  putStrLn $ show followTable
printRegexTables _ = do return ()
