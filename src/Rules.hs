{-
Copyright (C) 2013 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Rules (parse) where

import Data.Monoid
import Text.Trifecta

type Name = String
data CharacterOrRange = Character Char | Range Char Char deriving (Show)
data Rule = Class Name [CharacterOrRange] deriving (Show)

parse :: String -> Either String [Rule]
parse input =
  case parseString (some parseRule) mempty input of
    (Success a) -> Right a
    a -> Left (show a)

parseRule :: Parser Rule
parseRule = do
  res <- choice [parseClass]
  _ <- some newline
  return res

parseClass :: Parser Rule
parseClass = do
  _ <- string "class"
  _ <- some space
  name <- manyTill anyChar (some space)
  classContents <-
    char '[' >> manyTill (choice [try parseCharRange, try parseChar]) (char ']')
  return $ Class name classContents

parseCharRange :: Parser CharacterOrRange
parseCharRange = do
  a <- anyChar
  _ <- char '-'
  b <- anyChar
  return $ Range a b

parseChar :: Parser CharacterOrRange
parseChar = do
  _ <- optional (char '\\')
  a <- anyChar
  return $ Character a
