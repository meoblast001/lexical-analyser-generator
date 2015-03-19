{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

{-# LANGUAGE QuasiQuotes #-}

module Output.CPP (outputCpp) where

import Rules
import Text.RawString.QQ

outputCpp :: FilePath -> [Rule] -> IO ()
outputCpp baseName stateTable = do
  outputHeader (baseName ++ ".hpp")

outputHeader :: FilePath -> IO ()
outputHeader fileName = writeFile fileName lexicalAnalyserHpp

lexicalAnalyserHpp :: String
lexicalAnalyserHpp = [r|
#ifndef LEXICAL_ANALYZER_HPP
#define LEXICAL_ANALYZER_HPP

#include <iostream>
using namespace std;

class LexicalAnalyzer {
  public:
    LexicalAnalyzer(istream& input = cin);
    void start();
    bool next(Token& t, string& lexeme);
}

#endif
|]
