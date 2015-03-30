{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

{-# LANGUAGE QuasiQuotes #-}

module Output.CPP (outputCpp) where

import Data.List (intersperse, sortBy)
import FollowTable
import Rules
import StartEndTable
import StateTable
import Text.RawString.QQ

outputCpp :: FilePath -> [Rule] -> IO ()
outputCpp baseName rules = do
  outputHeader (baseName ++ ".hpp") rules
  outputSource (baseName ++ ".cpp") (baseName ++ ".hpp") rules

outputSource :: FilePath -> FilePath -> [Rule] -> IO ()
outputSource fileName headerPath rules =
  let include = includeHeader headerPath
      tokenDefinitions = defineTokens [name | (Token name _) <- rules]
      transTables = rulesToCpp rules 1
      tokenRulesList = rulesList "tokens" [x | x@(Token _ _) <- rules]
      ignoreRulesList = rulesList "ignores" [x | x@(Ignore _) <- rules]
  in writeFile fileName (include ++ sourceClasses ++ tokenDefinitions ++
                         transTables ++ tokenRulesList ++ ignoreRulesList ++
                         lexicalAnalyserCpp)

outputHeader :: FilePath -> [Rule] -> IO ()
outputHeader fileName rules =
  let tokenNames = [name | (Token name _) <- rules]
  in writeFile fileName $ lexicalAnalyserHpp tokenNames

includeHeader :: FilePath -> String
includeHeader headerPath = [r|
#include <cstdio>
#include <stdexcept>
#include "|] ++ headerPath ++ [r|"
|]

sourceClasses :: String
sourceClasses = [r|
const int END = -1;

struct CharOrRange {
  bool end;
  bool is_range;
  char c1, c2;
  CharOrRange() : end(true) { }
  CharOrRange(char c1) : end(false), is_range(false), c1(c1) { }
  CharOrRange(char c1, char c2) : end(false), is_range(true), c1(c1), c2(c2) { }
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
  StateTransition(int goto_state) : type(ANYCHAR), goto_state(goto_state) { }
  StateTransition() : type(NONE) { }
};

struct Rule {
  bool end;
  Token token;
  StateTransition* transition_table;
  int num_transitions;
  int* accepting_states;

  Rule(StateTransition* transition_table, int num_transitions,
       int* accepting_states) :
    end(false), transition_table(transition_table),
    num_transitions(num_transitions), accepting_states(accepting_states) { }
  Rule(Token token, StateTransition* transition_table, int num_transitions,
       int* accepting_states) :
    end(false), token(token), transition_table(transition_table),
    num_transitions(num_transitions), accepting_states(accepting_states) { }
  Rule() : end(true) { }
};
|]

defineTokens :: [Name] -> String
defineTokens rules =
  let defineToken name num = [r|Token |] ++ name ++ [r| = |] ++ show num ++
                             [r|;|]
  in concat (intersperse "\n" $ zipWith defineToken rules [1..])

rulesToCpp :: [Rule] -> Integer -> String
rulesToCpp ((Class name charsAndRanges):rest) i = [r|
CharOrRange charclass_|] ++ name ++ [r|[] =
{|] ++ concat (intersperse ", " (map charOrRangeToCpp charsAndRanges)) ++ [r|,
 CharOrRange()};
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

charOrRangeToCpp :: CharacterOrRange -> String
charOrRangeToCpp (Character c) = [r|CharOrRange('|] ++ [c] ++ [r|')|]
charOrRangeToCpp (Range c1 c2) = [r|CharOrRange('|] ++ [c1] ++ [r|', '|] ++
                                 [c2] ++ [r|')|]

transitionTable :: Name -> StateTable -> String
transitionTable name stateTable =
  let rowLength = maxStateTransitions stateTable
      getTransitions (StateTableEntry _ _ transitions) = transitions
      transRows = map transAnyLast
                      (map getTransitions $ stateTableEntries stateTable)
  in [r|
StateTransition trans_|] ++ name ++ [r|[][|] ++ show rowLength ++ [r|] = { {
  |] ++ concat (intersperse "}, {" $ map (transitionRow rowLength) transRows) ++
  [r|
} };
int num_trans_|] ++ name ++ [r| = |] ++ show rowLength ++ [r|;
|]

transAnyLast :: [StateTransition] -> [StateTransition]
transAnyLast transitions =
  let anyLast (RxAnyChar _) (RxAnyChar _) = EQ
      anyLast (RxAnyChar _) _ = GT
      anyLast _ (RxAnyChar _) = LT
      anyLast _ _ = EQ
      anyLastTransition (StateTransition rx1 _) (StateTransition rx2 _) =
        anyLast rx1 rx2
  in sortBy anyLastTransition transitions

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
     concat (intersperse ", " (map toStateNum accepting)) ++ [r|, END}; |]

rulesList :: Name -> [Rule] -> String
rulesList name rules =
  let toRule (Token name _) _ = [r|Rule(|] ++ name ++ [r|, &trans_|] ++ name ++
                                [r|[0][0], num_trans_|] ++ name ++
                                [r|, accepting_|] ++ name ++ [r|)|]
      toRule (Ignore _) id = [r|Rule(&trans_|] ++ show id ++
                             [r|[0][0], num_trans_|] ++ show id ++
                             [r|, accepting_|] ++ show id ++ [r|)|]
      toRule _ _ = ""
      ruleStrings = filter (not . null) $ zipWith toRule rules [1..]
  in [r|
Rule rules_|] ++ name ++ [r|[] = {
  |] ++ concat (intersperse ", " ruleStrings) ++ [r|,
  Rule()
}; |]

lexicalAnalyserCpp :: String
lexicalAnalyserCpp = [r|
bool isAccepting(int accepting[], int current_state) {
  for (int i = 0; accepting[i] != END; ++i) {
    if (accepting[i] == current_state)
      return true;
  }
  return false;
}

enum DFAMatchResult {
  MATCHED,
  FAILED,
  REACHED_EOF,
};

DFAMatchResult dfaMatch(Rule& rule, istream& input, string& lexeme) {
  streampos start_pos = input.tellg();
  int current_state = 0;
  char current_char = (char) input.get();
  string found_chars;
  bool made_transition = true;
  while (made_transition && current_char != EOF) {
    made_transition = false;
    for (int i = 0; i < rule.num_transitions && !made_transition; ++i) {
      StateTransition& cur_transition =
        rule.transition_table[(current_state * rule.num_transitions) + i];
      switch (cur_transition.type) {
        case CHARACTER: {
          if (current_char == cur_transition.character) {
            current_state = cur_transition.goto_state;
            made_transition = true;
          }
          break;
        }
        case CHARCLASS: {
          bool success = false;
          for (int j = 0; !cur_transition.charclass[j].end; ++j) {
            CharOrRange& cur_char_or_range = cur_transition.charclass[j];
            if (cur_char_or_range.is_range) {
              if (current_char >= cur_char_or_range.c1 &&
                  current_char <= cur_char_or_range.c2)
                success = true;
            } else {
              if (current_char == cur_char_or_range.c1)
                success = true;
            }
          }
          if (success) {
            current_state = cur_transition.goto_state;
            made_transition = true;
          }
          break;
        }
        case ANYCHAR: {
          current_state = cur_transition.goto_state;
          made_transition = true;
          break;
        }
        case NONE:
          break;
      }
    }
    if (made_transition)
      found_chars += current_char;
    current_char = (char) input.get();
  }
  input.seekg(start_pos, input.beg);
  if (isAccepting(rule.accepting_states, current_state)) {
    lexeme = found_chars;
    return MATCHED;
  } else if (current_char == EOF)
    return REACHED_EOF;
  else {
    return FAILED;
  }
}

LexicalAnalyzer::LexicalAnalyzer(istream& input) : input(input) {}

void LexicalAnalyzer::start()
{
  input.seekg(0, input.beg);
}

bool LexicalAnalyzer::next(Token& t, string& lexeme) {
  string current_lexeme;
  bool reached_eof = false;

  do {
    lexeme = "";
    for (int i = 0; !rules_ignores[i].end; ++i) {
      switch (dfaMatch(rules_ignores[i], input, current_lexeme)) {
        case MATCHED:
          if (current_lexeme.length() > lexeme.length())
            lexeme = current_lexeme;
          break;
        case FAILED:
          break;
        case REACHED_EOF:
          reached_eof = true;
      }
      current_lexeme = "";
    }
    if (lexeme.length() > 0)
      input.seekg(streamoff(lexeme.length()), input.cur);
    else if (reached_eof)
      return false;
    reached_eof = false;
  } while (lexeme.length() > 0);

  for (int i = 0; !rules_tokens[i].end; ++i) {
    switch (dfaMatch(rules_tokens[i], input, current_lexeme)) {
      case MATCHED:
        if (current_lexeme.length() > lexeme.length()) {
          t = rules_tokens[i].token;
          lexeme = current_lexeme;
        }
      case FAILED:
        break;
      case REACHED_EOF:
        reached_eof = true;
    }
  }
  if (lexeme.length() > 0) {
    input.seekg(streamoff(lexeme.length()), input.cur);
    return true;
  } else {
    if (reached_eof)
      return false;
    throw invalid_argument("Could not match a token in stream.");
  }
}
|]

lexicalAnalyserHpp :: [Name] -> String
lexicalAnalyserHpp tokenNames =
  let declareToken name = [r|extern Token |] ++ name ++ [r|;|]
  in [r|
#ifndef LEXICAL_ANALYZER_HPP
#define LEXICAL_ANALYZER_HPP

#include <iostream>
using namespace std;

typedef int Token;

|] ++ concat (intersperse "\n" $ map declareToken tokenNames) ++ [r|

class LexicalAnalyzer {
  public:
    LexicalAnalyzer(istream& input = cin);
    void start();
    bool next(Token& t, string& lexeme);
  private:
    istream& input;
};

#endif
|]

maxStateTransitions :: StateTable -> Integer
maxStateTransitions stateTable =
  let getTransitions (StateTableEntry _ _ transitions) = transitions
      transLists = map getTransitions $ stateTableEntries stateTable
  in maximum $ map (fromIntegral . length) transLists
