{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

{-# LANGUAGE QuasiQuotes #-}

module Output.CPP (outputCpp) where

import Data.List (intersperse)
import FollowTable
import Rules
import StartEndTable
import StateTable
import Text.RawString.QQ

outputCpp :: FilePath -> [Rule] -> IO ()
outputCpp baseName rules = do
  outputHeader (baseName ++ ".hpp")
  outputSource (baseName ++ ".cpp") (baseName ++ ".hpp") rules

outputSource :: FilePath -> FilePath -> [Rule] -> IO ()
outputSource fileName headerPath rules =
  let include = includeHeader headerPath
      transTables = rulesToCpp rules 1
  in writeFile fileName (include ++ sourceClasses ++ transTables ++
                         lexicalAnalyserCpp)

outputHeader :: FilePath -> IO ()
outputHeader fileName = writeFile fileName lexicalAnalyserHpp

includeHeader :: FilePath -> String
includeHeader headerPath = "#include \"" ++ headerPath ++ "\""

sourceClasses :: String
sourceClasses = [r|
struct CharOrRange {
  bool end;
  bool is_range;
  char c1, c2;
  CharOrRange(char c1) : end(false), is_range(false), c1(c1) { }
  CharOrRange(char c1, char c2) : end(false), is_range(true), c1(c1), c2(c2) { }
  CharOrRange() : end(true) { }
};

enum TransitionType {
  NONE,
  CHARACTER,
  CHARCLASS,
  ANYCHAR,
};

struct StateTransition {
  TransitionType type;
  char character;
  CharOrRange* charclass;
  int goto_state;

  StateTransition(char c, int goto_state) :
    type(CHARACTER), character(c), charclass(0), goto_state(goto_state) { }
  StateTransition(CharOrRange* c, int goto_state) :
    type(CHARCLASS), character(0), charclass(c), goto_state(goto_state) { }
  StateTransition(int goto_state) : type(ANYCHAR) { }
  StateTransition() : type(NONE) { }
};
|]

rulesToCpp :: [Rule] -> Integer -> String
rulesToCpp ((Class name charsAndRanges):rest) i = [r|
CharOrRange charclass_|] ++ name ++ [r|[] = {};
|] ++ rulesToCpp rest i
rulesToCpp ((Token name regex):rest) i =
  let seTable = buildStartEndTable regex
      followTable = buildFollowTable seTable
      stateTable = buildStateTable (head $ seTableEntries seTable) followTable
  in transitionTable name stateTable ++ acceptingStates name stateTable ++
     rulesToCpp rest i
rulesToCpp ((Ignore regex):rest) i =
  let seTable = buildStartEndTable regex
      followTable = buildFollowTable seTable
      stateTable = buildStateTable (head $ seTableEntries seTable) followTable
  in transitionTable (show i) stateTable ++
     acceptingStates (show i) stateTable ++ rulesToCpp rest (i + 1)
rulesToCpp [] _ = []

transitionTable :: Name -> StateTable -> String
transitionTable name stateTable =
  let rowLength = maxStateTransitions stateTable
      getTransitions (StateTableEntry _ _ transitions) = transitions
      transRows = map getTransitions $ stateTableEntries stateTable
  in [r|
StateTransition trans_|] ++ name ++ [r|[][|] ++ show rowLength ++ [r|] = { {
  |] ++ concat (intersperse "}, {" $ map (transitionRow rowLength) transRows) ++
  [r|
} };
|]

transitionRow :: Integer -> [StateTransition] -> String
transitionRow rowLength ((StateTransition (RxChar c _) num):rest) = [r|
  StateTransition('|] ++ toEscapeSequence c ++ [r|', |]  ++ show num ++ [r|),
|] ++ transitionRow (rowLength - 1) rest
transitionRow rowLength ((StateTransition (RxClass name _) num):rest) = [r|
  StateTransition(charclass_|] ++ name ++ [r|, |] ++ show num ++ [r|),
|] ++ transitionRow (rowLength - 1) rest
transitionRow rowLength ((StateTransition (RxAnyChar _) num):rest) = [r|
  StateTransition(|] ++ show num ++ [r|),
|] ++ transitionRow (rowLength - 1) rest
transitionRow rowLength (item:rest) = transitionRow (rowLength - 1) rest
transitionRow rowLength []
  | rowLength > 0 = [r|StateTransition(),|] ++ transitionRow (rowLength - 1) []
  | otherwise = []

acceptingStates :: Name -> StateTable -> String
acceptingStates name stateTable =
  let accepting = filter isAcceptingState $ stateTableEntries stateTable
      toStateNum (StateTableEntry num _ _) = show num
  in [r|int accepting_|] ++ name ++ [r|[] = {|] ++
     concat (intersperse "," (map toStateNum accepting)) ++ [r|};|]

lexicalAnalyserCpp :: String
lexicalAnalyserCpp = [r|
LexicalAnalyzer::LexicalAnalyzer(istream& input) {}
void LexicalAnalyzer::start() { }
bool next(Token& t, string& lexeme) { return false; }
|]

lexicalAnalyserHpp :: String
lexicalAnalyserHpp = [r|
#ifndef LEXICAL_ANALYZER_HPP
#define LEXICAL_ANALYZER_HPP

#include <iostream>
using namespace std;

typedef int Token;

class LexicalAnalyzer {
  public:
    LexicalAnalyzer(istream& input = cin);
    void start();
    bool next(Token& t, string& lexeme);
};

#endif
|]

maxStateTransitions :: StateTable -> Integer
maxStateTransitions stateTable =
  let getTransitions (StateTableEntry _ _ transitions) = transitions
      transLists = map getTransitions $ stateTableEntries stateTable
  in maximum $ map (fromIntegral . length) transLists
