{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module StateTable
( StateNum
, StateTransition(..)
, StateTableEntry(..)
, StateTable(..)
, buildStateTable
, isAcceptingState
) where

import Data.Functor
import Data.List
import Data.Maybe
import FollowTable
import Rules
import StartEndTable

type StateNum = Integer
data StateTransition = StateTransition Regex StateNum deriving (Show)
data StateTableEntry = StateTableEntry StateNum [Regex] [StateTransition]
  deriving (Show)
newtype StateTable = StateTable { stateTableEntries :: [StateTableEntry] }

buildStateTable :: StartEndTableEntry -> FollowTable -> StateTable
buildStateTable seTableEntry followTable =
  let incompEntries = buildIncompEntries seTableEntry followTable
      compEntries = completeEntries followTable incompEntries
  in StateTable compEntries

completeEntries :: FollowTable -> [IncompleteEntry] -> [StateTableEntry]
completeEntries _ [] = []
completeEntries followTable incompEntries =
  let incompEntry@(IncompleteEntry _ regexes):rest = incompEntries
      transitions = catMaybes $ map (buildTransition followTable incompEntries)
                                    regexes
  in (completeEntry incompEntry transitions):(completeEntries followTable rest)

buildTransition :: FollowTable -> [IncompleteEntry] -> Regex ->
                   Maybe StateTransition
buildTransition followTable incompEntries regex =
  let follows = followVal <$> searchByKey followTable regex
      transState = findStateByRegexes incompEntries <$> follows
      transStateNum = (\(IncompleteEntry sn _) -> sn) <$> transState
  in StateTransition regex <$> transStateNum

findStateByRegexes :: [IncompleteEntry] -> [Regex] -> IncompleteEntry
findStateByRegexes (cur@(IncompleteEntry _ curRegexes):rest) regexes =
  if curRegexes == regexes then cur else findStateByRegexes rest regexes

data IncompleteEntry = IncompleteEntry StateNum [Regex] deriving (Show)
completeEntry :: IncompleteEntry -> [StateTransition] -> StateTableEntry
completeEntry (IncompleteEntry stateNum regex) transitions =
  StateTableEntry stateNum regex transitions

buildIncompEntries :: StartEndTableEntry -> FollowTable -> [IncompleteEntry]
buildIncompEntries (StartEndTableEntry _ _ first _) followTable =
  let firstEntry = IncompleteEntry 0 first
  in firstEntry:(buildIncompFolStates followTable [] [firstEntry] 1)

buildIncompFolStates :: FollowTable -> [IncompleteEntry] -> [IncompleteEntry] ->
                        StateNum -> [IncompleteEntry]
buildIncompFolStates _ _ [] _ = []
buildIncompFolStates followTable oldEntries newEntries stateNum =
  let allEntries = oldEntries ++ newEntries
      newRegexes = nub $ concatMap (\(IncompleteEntry _ r) -> r) newEntries
      follows = map followVal $ catMaybes $ map (searchByKey followTable)
                                                newRegexes
      statesExist = doStatesExist allEntries follows
      followedStates = buildFollowedStates statesExist stateNum
      newStateNum = nextStateNum followedStates
  in followedStates ++
     buildIncompFolStates followTable allEntries followedStates newStateNum

doStatesExist :: [IncompleteEntry] -> [[Regex]] -> [([Regex], Bool)]
doStatesExist _ [] = []
doStatesExist allStates (rxCur:rxRest) =
  let isMatchingState (IncompleteEntry _ regexes) = regexes == rxCur
      exists = isJust $ find isMatchingState allStates
  in (rxCur, exists):(doStatesExist allStates rxRest)

nextStateNum :: [IncompleteEntry] -> StateNum
nextStateNum entries =
  let compareEntries (IncompleteEntry a _) (IncompleteEntry b _) = a `compare` b
      (IncompleteEntry lastNum _) = last $ sortBy compareEntries entries
  in lastNum + 1

buildFollowedStates :: [([Regex], Bool)] -> StateNum -> [IncompleteEntry]
buildFollowedStates [] _ = []
buildFollowedStates ((_, True):rest) stNum = buildFollowedStates rest stNum
buildFollowedStates ((regexes, False):rest) stNum =
  (IncompleteEntry stNum regexes):(buildFollowedStates rest (stNum + 1))

isAcceptingState :: StateTableEntry -> Bool
isAcceptingState (StateTableEntry _ regexes _) =
  let isEnd (RxEnd) = True
      isEnd _ = False
  in any isEnd regexes
