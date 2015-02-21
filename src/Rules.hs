{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Rules (parse) where

import Control.Monad
import Data.Functor
import Data.Monoid
import Text.Parser.LookAhead
import Text.Trifecta

type Name = String
data CharacterOrRange = Character Char | Range Char Char deriving (Show)
data Regex = RxChar Char | RxClass Name | RxMany Regex | RxSome Regex |
             RxOptional Regex | RxAnd Regex Regex | RxOr Regex Regex
             deriving (Show)
data Rule = Class Name [CharacterOrRange] | Token Name Regex | Ignore Regex
            deriving (Show)

parse :: String -> Either String [Rule]
parse input =
  case parseString parseRules mempty input of
    (Success a) -> Right a
    a -> Left (show a)

parseRules :: Parser [Rule]
parseRules = skipMany (try lineEndWhitespace) >> some parseRule

parseRule :: Parser Rule
parseRule = do
  res <- choice [parseClass, parseToken, parseIgnore]
  _ <- skipMany (try lineEndWhitespace)
  return res

lineEndWhitespace :: Parser ()
lineEndWhitespace = many (choice [char ' ', char '\t']) >> optional comment >>
                    newline >> return ()

comment :: Parser String
comment = string "//" >> manyTill anyChar (lookAhead newline)

parseClass :: Parser Rule
parseClass = do
  _ <- string "class"
  _ <- some space
  name <- manyTill anyChar (some space)
  classContents <-
    char '[' >> manyTill (choice [try parseCharRange, try parseChar]) (char ']')
  return $ Class name classContents

parseToken :: Parser Rule
parseToken = do
  _ <- string "token"
  _ <- some space
  name <- manyTill anyChar (some space)
  regex <- parseRegex
  return $ Token name regex

parseIgnore :: Parser Rule
parseIgnore = do
  _ <- string "ignore"
  _ <- some space
  regex <- parseRegex
  return $ Ignore regex

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

parseRegex :: Parser Regex
parseRegex = parseRegexPartsUntil (lookAhead lineEndWhitespace)

parseRegexPartsUntil :: Parser a -> Parser Regex
parseRegexPartsUntil end =
  let partTypes = [parseRxParens, parseRxClass, parseRxChar]
      -- Array of "or" parts containing arrays of "and" parts.
      parts = manyTill (manyTill (choice partTypes)
                                 (choice [void $ char '|', void end])) end
  in foldr1 RxOr <$> (map (foldr1 RxAnd) <$> parts)

parseRxParens :: Parser Regex
parseRxParens = do
  _ <- char '('
  inside <- parseRegexPartsUntil (lookAhead $ char ')')
  _ <- char ')'
  parseMaybeRxClosure inside

parseRxClass :: Parser Regex
parseRxClass = do
  classname <- char '[' >> manyTill anyChar (try $ char ']')
  parseMaybeRxClosure (RxClass classname)

parseRxChar :: Parser Regex
parseRxChar = do
  escape <- optional $ char '\\'
  c <- (\c -> maybe c (const $ rxEscapeCode c) escape) <$> anyChar
  parseMaybeRxClosure (RxChar c)

rxEscapeCode :: Char -> Char
rxEscapeCode 'n' = '\n'
rxEscapeCode 'r' = '\r'
rxEscapeCode 'f' = '\f'
rxEscapeCode 't' = '\t'
rxEscapeCode a = a

parseMaybeRxClosure :: Regex -> Parser Regex
parseMaybeRxClosure regex =
  let toRegex rx '*' = RxMany rx
      toRegex rx '+' = RxSome rx
      toRegex rx '?' = RxOptional rx
      toRegex _ _ = error "An error happened that should never happen."
  in do
    operator <- optional (try $ choice [char '*', char '+', char '?'])
    return $ maybe regex (toRegex regex) operator
