{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Rules
( Name
, CharacterOrRange(..)
, Regex(..)
, Rule(..)
, parse
, toEscapeSequence
, showRegexType
) where

import Control.Monad
import Data.Functor
import Data.Monoid
import Text.Parser.LookAhead
import Text.Trifecta

type Id = Integer
type Name = String
data CharacterOrRange = Character Char | Range Char Char deriving (Show)
data RegexNoId = RxNChar Char | RxNClass Name | RxNAnyChar | RxNMany RegexNoId |
                 RxNSome RegexNoId | RxNOptional RegexNoId |
                 RxNAnd RegexNoId RegexNoId | RxNOr RegexNoId RegexNoId
                 deriving (Eq, Show)
data Regex = RxChar Char Id | RxClass Name Id | RxAnyChar Id | RxMany Regex |
             RxSome Regex | RxOptional Regex | RxAnd Regex Regex |
             RxOr Regex Regex | RxEnd deriving (Eq, Show)
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
parseRegex = withRxEnd <$> (fst . rxNToRx 1) <$>
             parseRegexPartsUntil (lookAhead lineEndWhitespace)

parseRegexPartsUntil :: Parser a -> Parser RegexNoId
parseRegexPartsUntil end =
  let partTypes = [parseRxParens, parseRxClass, parseRxAnyChar, parseRxChar]
      -- Array of "or" parts containing arrays of "and" parts.
      parts = manyTill (manyTill (choice partTypes)
                                 (choice [void $ char '|', void end])) end
  in foldr1 RxNOr <$> (map (foldr1 RxNAnd) <$> parts)

parseRxParens :: Parser RegexNoId
parseRxParens = do
  _ <- char '('
  inside <- parseRegexPartsUntil (lookAhead $ char ')')
  _ <- char ')'
  parseMaybeRxClosure inside

parseRxClass :: Parser RegexNoId
parseRxClass = do
  classname <- char '[' >> manyTill anyChar (try $ char ']')
  parseMaybeRxClosure (RxNClass classname)

parseRxAnyChar :: Parser RegexNoId
parseRxAnyChar = char '.' >> parseMaybeRxClosure RxNAnyChar

parseRxChar :: Parser RegexNoId
parseRxChar = do
  escape <- optional $ char '\\'
  c <- (\c -> maybe c (const $ rxEscapeCode c) escape) <$> anyChar
  parseMaybeRxClosure (RxNChar c)

rxEscapeCode :: Char -> Char
rxEscapeCode 'n' = '\n'
rxEscapeCode 'r' = '\r'
rxEscapeCode 'f' = '\f'
rxEscapeCode 't' = '\t'
rxEscapeCode a = a

toEscapeSequence :: Char -> String
toEscapeSequence '\n' = "\\n"
toEscapeSequence '\r' = "\\r"
toEscapeSequence '\f' = "\\f"
toEscapeSequence '\t' = "\\t"
toEscapeSequence a = [a]

parseMaybeRxClosure :: RegexNoId -> Parser RegexNoId
parseMaybeRxClosure regex =
  let toRegex rx '*' = RxNMany rx
      toRegex rx '+' = RxNSome rx
      toRegex rx '?' = RxNOptional rx
      toRegex _ _ = error "An error happened that should never happen."
  in do
    operator <- optional (try $ choice [char '*', char '+', char '?'])
    return $ maybe regex (toRegex regex) operator

rxNToRx :: Id -> RegexNoId -> (Regex, Id)
rxNToRx id (RxNMany r) =
  let (rNew, newId) = rxNToRx id r
  in (RxMany rNew, newId)
rxNToRx id (RxNSome r) =
  let (rNew, newId) = rxNToRx id r
  in (RxSome rNew, newId)
rxNToRx id (RxNOptional r) =
  let (rNew, newId) = rxNToRx id r
  in (RxOptional rNew, newId)
rxNToRx id (RxNAnd r1 r2) =
  let (r1New, r1NewId) = rxNToRx id r1
      (r2New, r2NewId) = rxNToRx r1NewId r2
  in (RxAnd r1New r2New, r2NewId)
rxNToRx id (RxNOr r1 r2) =
  let (r1New, r1NewId) = rxNToRx id r1
      (r2New, r2NewId) = rxNToRx r1NewId r2
  in (RxOr r1New r2New, r2NewId)
rxNToRx id (RxNChar a) = (RxChar a id, id + 1)
rxNToRx id (RxNClass a) = (RxClass a id, id + 1)
rxNToRx id (RxNAnyChar) = (RxAnyChar id, id + 1)

withRxEnd :: Regex -> Regex
withRxEnd regex = RxAnd regex RxEnd

showRegexType :: Regex -> String
showRegexType rx@(RxMany _) = "*"
showRegexType rx@(RxSome _) = "+"
showRegexType rx@(RxOptional _) = "?"
showRegexType rx@(RxAnd _ _) = "And"
showRegexType rx@(RxOr _ _) = "Or"
showRegexType rx@(RxChar a id) = "Char (" ++ show id ++ "): " ++ show a
showRegexType rx@(RxClass a id) = "Class (" ++ show id ++ "): " ++ show a
showRegexType rx@(RxAnyChar id) = "Any char (" ++ show id ++ ")"
showRegexType rx@(RxEnd) = "End"
