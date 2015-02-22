{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module StartEndTable
( Nullable
, StartEndTableEntry(..)
, StartEndTable(..)
, buildStartEndTable
) where

import Data.Functor
import Data.List
import Rules

type Nullable = Bool
data StartEndTableEntry = StartEndTableEntry Regex Nullable [Regex] [Regex]
newtype StartEndTable = StartEndTable [StartEndTableEntry]

instance Show StartEndTableEntry where
  show entry@(StartEndTableEntry rx nullable r1 r2) =
    showRegexType rx ++ " | " ++ show nullable ++ " | " ++
    concat ((++ " ") <$> showRegexType <$> r1) ++ "| " ++
    concat ((++ " ") <$> showRegexType <$> r2)

instance Show StartEndTable where
  show table@(StartEndTable (x:xs)) = show x ++ "\n" ++
                                      (show $ StartEndTable xs)
  show table@(StartEndTable []) = []

buildStartEndTable :: Regex -> StartEndTable
buildStartEndTable regex =
  let operators = findRxOperators regex
      entries = map rxOperatorToEntry operators
  in StartEndTable entries

findRxOperators :: Regex -> [Regex]
findRxOperators rx@(RxMany r) = rx:(findRxOperators r)
findRxOperators rx@(RxSome r) = rx:(findRxOperators r)
findRxOperators rx@(RxOptional r) = rx:(findRxOperators r)
findRxOperators rx@(RxAnd r1 r2) = rx:(findRxOperators r1 ++ findRxOperators r2)
findRxOperators rx@(RxOr r1 r2) = rx:(findRxOperators r1 ++ findRxOperators r2)
findRxOperators _ = []

rxOperatorToEntry :: Regex -> StartEndTableEntry
rxOperatorToEntry rx = StartEndTableEntry rx (isNullable rx) (firstRx rx)
                                          (lastRx rx)

isNullable :: Regex -> Bool
isNullable rx@(RxChar _) = False
isNullable rx@(RxClass _) = False
isNullable rx@(RxMany _) = True
isNullable rx@(RxSome r) = isNullable r
isNullable rx@(RxOptional _) = True
isNullable rx@(RxAnd r1 r2) = and $ map isNullable [r1, r2]
isNullable rx@(RxOr r1 r2) = or $ map isNullable [r1, r2]

firstRx :: Regex -> [Regex]
firstRx rx@(RxChar _) = [rx]
firstRx rx@(RxClass _) = [rx]
firstRx rx@(RxMany r) = firstRx r
firstRx rx@(RxSome r) = firstRx r
firstRx rx@(RxOptional r) = firstRx r
firstRx rx@(RxAnd r1 r2) = if isNullable r1 then union (firstRx r1) (firstRx r2)
                                            else firstRx r1
firstRx rx@(RxOr r1 r2) = union (firstRx r1) (firstRx r2)

lastRx :: Regex -> [Regex]
lastRx rx@(RxChar _) = [rx]
lastRx rx@(RxClass _) = [rx]
lastRx rx@(RxMany r) = lastRx r
lastRx rx@(RxSome r) = lastRx r
lastRx rx@(RxOptional r) = lastRx r
lastRx rx@(RxAnd r1 r2) = if isNullable r2 then union (lastRx r1) (lastRx r2)
                                           else lastRx r2
lastRx rx@(RxOr r1 r2) = union (lastRx r1) (lastRx r2)
