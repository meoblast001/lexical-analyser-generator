{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Main where

import FollowTable
import Output.CPP
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
    [input, outputBase] -> process input outputBase
    _ -> error "Please provide an input file name."

-- Generate C++ code from rules file.

process :: FilePath -> FilePath -> IO ()
process inputFile outputBase = do
  fileHandle <- openFile inputFile ReadMode
  contents <- hGetContents fileHandle
  either (putStrLn . show) (outputCpp outputBase) (parse contents)
  hClose fileHandle

-- Display information about the process of creating the DFA for a rules file.

displayInfo :: FilePath -> IO ()
displayInfo filename = do
  fileHandle <- openFile filename ReadMode
  contents <- hGetContents fileHandle
  either (putStrLn . show) displayRulesInfo (parse contents)
  hClose fileHandle

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
      states = stateTableEntries $
               buildStateTable (head $ seTableEntries seTable) followTable
  putStrLn ("Start-end table for \"" ++ name ++ "\":")
  putStrLn $ show seTable
  putStrLn ("Follow table for \"" ++ name ++ "\":")
  putStrLn $ show followTable
  putStrLn ("State table for \"" ++ name ++ "\":")
  mapM_ (putStrLn . show) states >> putStr "\n"
printRegexTables ignore@(Ignore rx) = do
  let seTable = buildStartEndTable rx
      followTable = buildFollowTable seTable
      states = stateTableEntries $
               buildStateTable (head $ seTableEntries seTable) followTable
  putStrLn "Start-end table for ignore:"
  putStrLn $ show seTable
  putStrLn "Follow table for ignore:"
  putStrLn $ show followTable
  putStrLn ("State table for ignore")
  mapM_ (putStrLn . show) states >> putStr "\n"
printRegexTables _ = do return ()
