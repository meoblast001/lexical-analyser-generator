{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module StateTable
( StateTableEntry(..)
, StateTable(..)
, buildIncompEntries
) where

import Data.List
import Data.Maybe
import FollowTable
import Rules
import StartEndTable

type StateNum = Integer
data StateTransition = StateTransition Regex StateNum
data StateTableEntry = StateTableEntry StateNum [Regex] [StateTransition]
newtype StateTable = StateTable { stateTableEntries :: [StateTableEntry] }

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
