{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Rules
( Id
, Name
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
-- |Represents either a single character or a range of characters.
data CharacterOrRange = Character Char | Range Char Char deriving (Show)
-- |Regex parts before IDs are assigned. These should be converted to 'Regex'es
-- before leaving the parser so that separate regex parts representing the same
-- data can be distinguished.
data RegexNoId = RxNChar Char | RxNClass Name | RxNAnyChar | RxNMany RegexNoId |
                 RxNSome RegexNoId | RxNOptional RegexNoId |
                 RxNAnd RegexNoId RegexNoId | RxNOr RegexNoId RegexNoId
                 deriving (Eq, Show)
-- |Recursive structures containing parts of regexes.
data Regex = RxChar Char Id | RxClass Name Id | RxAnyChar Id | RxMany Regex |
             RxSome Regex | RxOptional Regex | RxAnd Regex Regex |
             RxOr Regex Regex | RxEnd deriving (Eq, Show)
-- |Rule which was read from input rules file.
data Rule = Class Name [CharacterOrRange] | Token Name Regex | Ignore Regex
            deriving (Show)

-- |Parses a string of rules and produces either an error message or a list of
-- rules.
parse :: String -> Either String [Rule]
parse input =
  case parseString parseRules mempty input of
    (Success a) -> Right a
    a -> Left (show a)

-- |Matches the rules file format.
parseRules :: Parser [Rule]
parseRules =
  -- Possibly lines of whitespace followed by at least one parse rule.
  skipMany (try lineEndWhitespace) >> some parseRule

-- |Matches one rule and ignores whitespace after it.
parseRule :: Parser Rule
parseRule = do
  res <- choice [parseClass, parseToken, parseIgnore]
  -- Parse whitespace at the end of a rules line, followed possibly by lines of
  -- all whitespace.
  _ <- skipMany (try lineEndWhitespace)
  return res

-- |Matches spaces followed by an optional comment and a new line.
lineEndWhitespace :: Parser ()
lineEndWhitespace = many (choice [char ' ', char '\t']) >> optional comment >>
                    newline >> return ()

-- |Matches "//" followed by text until a newline. Does not consume newline.
comment :: Parser String
comment = string "//" >> manyTill anyChar (lookAhead newline)

-- |Matches a class definition.
parseClass :: Parser Rule
parseClass = do
  _ <- string "class"
  _ <- some space
  name <- manyTill anyChar (some space)
  classContents <-
    char '[' >> manyTill (choice [try parseCharRange, try parseChar]) (char ']')
  return $ Class name classContents

-- |Matches a token definition.
parseToken :: Parser Rule
parseToken = do
  _ <- string "token"
  _ <- some space
  name <- manyTill anyChar (some space)
  regex <- parseRegex
  return $ Token name regex

-- |Matches an ignore definition.
parseIgnore :: Parser Rule
parseIgnore = do
  _ <- string "ignore"
  _ <- some space
  regex <- parseRegex
  return $ Ignore regex

-- |Matches a range of form "a-z" for class definitions and returns as a
-- 'CharacterOrRange'.
parseCharRange :: Parser CharacterOrRange
parseCharRange = do
  a <- anyChar
  _ <- char '-'
  b <- anyChar
  return $ Range a b

-- |Matches a character for class definitions and returns as a
-- 'CharacterOrRange'. If prepended by "\", the character following is escaped.
parseChar :: Parser CharacterOrRange
parseChar = do
  _ <- optional (char '\\')
  a <- anyChar
  return $ Character a

-- |Matches a regex until line-end whitespace is found. That whitespace is not
-- consumed.
parseRegex :: Parser Regex
parseRegex = withRxEnd <$> (fst . rxNToRx 1) <$>
             parseRegexPartsUntil (lookAhead lineEndWhitespace)

-- |Matches a sequence of regex parts until the parser in the first parameter is
-- matched (and consumed). The returned value is the root of the regex, which
-- recursively contains the rest of the regex parts.
parseRegexPartsUntil :: Parser a -> Parser RegexNoId
parseRegexPartsUntil end =
  let partTypes = [parseRxParens, parseRxClass, parseRxAnyChar, parseRxChar]
      -- Array of "or" parts containing arrays of "and" parts.
      parts = manyTill (manyTill (choice partTypes)
                                 (choice [void $ char '|', void end])) end
  in foldr1 RxNOr <$> (map (foldr1 RxNAnd) <$> parts)

-- |Matches a regex inside of parentheses (optionally with closure).
parseRxParens :: Parser RegexNoId
parseRxParens = do
  _ <- char '('
  inside <- parseRegexPartsUntil (lookAhead $ char ')')
  _ <- char ')'
  parseMaybeRxClosure inside

-- |Matches a class in a regex (optionally with closure).
parseRxClass :: Parser RegexNoId
parseRxClass = do
  classname <- char '[' >> manyTill anyChar (try $ char ']')
  parseMaybeRxClosure (RxNClass classname)

-- |Matches a "." in a regex to represent any character (optionally with
-- closure).
parseRxAnyChar :: Parser RegexNoId
parseRxAnyChar = char '.' >> parseMaybeRxClosure RxNAnyChar

-- |Match a character in a regex (optionally with closure). If prepended by "\",
-- the character following is escaped.
parseRxChar :: Parser RegexNoId
parseRxChar = do
  escape <- optional $ char '\\'
  c <- (\c -> maybe c (const $ rxEscapeCode c) escape) <$> anyChar
  parseMaybeRxClosure (RxNChar c)

-- |Converts characters representing control characters into those control
-- characters.
rxEscapeCode :: Char -> Char
rxEscapeCode 'n' = '\n'
rxEscapeCode 'r' = '\r'
rxEscapeCode 'f' = '\f'
rxEscapeCode 't' = '\t'
rxEscapeCode a = a

-- |Converts control codes into their escape sequences.
toEscapeSequence :: Char -> String
toEscapeSequence '\n' = "\\n"
toEscapeSequence '\r' = "\\r"
toEscapeSequence '\f' = "\\f"
toEscapeSequence '\t' = "\\t"
toEscapeSequence a = [a]

-- |Attempt to match a closure operator and wrap the parameter regex in that
-- closure. If no closure is found, just return the parameter regex.
parseMaybeRxClosure :: RegexNoId -> Parser RegexNoId
parseMaybeRxClosure regex =
  let toRegex rx '*' = RxNMany rx
      toRegex rx '+' = RxNSome rx
      toRegex rx '?' = RxNOptional rx
      toRegex _ _ = error "An error happened that should never happen."
  in do
    operator <- optional (try $ choice [char '*', char '+', char '?'])
    return $ maybe regex (toRegex regex) operator

-- |Recursively converts a 'RegexNoId' into a 'Regex', where each regex part
-- receives a unique ID. The first parameter is the first Id, and can safely be
-- any value. The return value is a tuple containing the result 'Regex' and the
-- next unused ID value.
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

-- |Append the end character to the end of the regex. Should only be applied to
-- the root node of the regex.
withRxEnd :: Regex -> Regex
withRxEnd regex = RxAnd regex RxEnd

-- |Alternative, compact string representation of 'Regex'.
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
