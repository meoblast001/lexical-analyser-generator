### The Generic Haskell Lexical Analyser Generator ###
No effort was put into the name of this project. It was written for a compiler
design course. Provide a rules file and it will output a library which
recognises those tokens in strings.

#### How To Use
  1. Install Cabal 1.10+ and GHC 7.6+.
  2. Run `cabal install`. If you are not in a Cabal sandbox, the executable
     `LexicalAnalyserGenerator` will be installed to an executable path.
  3. For some rules file `RULES` and some output base name `OUTPUT_BASE`, run
     `LexicalAnalyserGenerator RULES OUTPUT_BASE` to generate a lexer for those
     rules. The lexer files will be named `OUTPUT_BASE.cpp` and
     `OUTPUT_BASE.hpp`.
  4. Compile `OUTPUT_BASE.cpp` with your C++ compiler.

#### Rules File
Rules files contain character classes, tokens, and ignores.

##### Character classes
A character class defines characters which are all treated as the same (for
example: the set of all uppercase and lowercase letters).

A character class declaration has the form: `class classname [set]`, where
`classname` is the name of the class, and `set` is a sequence of:
  - Characters
  - Character ranges in the form "α-β", which matches all characters from α to β
    inclusively.

Any whitespace between the brackets is considered to be part of the set.
No punctuation is used to delimit items in the set. If punctuation is included
in the set, put it at the end.

##### Tokens
A token is any symbol which has significance to your program. It is constructed
from a sequence of characters in your input. A token declaration has the form:
`token tokenname regex`, where `tokenname` is the name of the token and `regex`
is a sequence of regular expression rules of the following form:

| Form            | String it accepts                                   |
|-----------------|-----------------------------------------------------|
| Any character α | α                                                   |
| [classname]     | Any character in the class                          |
| ab              | αβ where a accepts α and b accepts β                |
| (a)             | Any string accepted by a                            |
| a\*             | Zero or more consecutive strings accepted by a      |
| a+              | One or more consecutive strings accepted by a       |
| a?              | Either the empty string or any string accepted by r |

##### Ignore
Ignore sequences are skipped before finding the next token. They take the form
`ignore regex`, where `regex` takes the same form as in tokens.

#### Output Files
The program outputs C++ files with the following interface:

```
    class LexicalAnalyzer {
      public:
        LexicalAnalyzer(istream& input = cin);
        void start();
        bool next(Token& t, string& lexeme);
    }
```

Initialise objects of this class with an input stream. This input stream must be
rewindable (cin may not work). `start` rewinds the input to the beginning.
`next` will skip ignore sequences and find the next token and lexeme, returning
true on success, false on EOF, and throwing `invalid_argument` if no token is
found.
