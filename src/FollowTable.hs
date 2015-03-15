{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module FollowTable
( FollowTableEntry(..)
, FollowTable(..)
, buildFollowTable
, searchByKey
) where

import Data.Functor
import Data.List
import Data.Maybe
import Rules
import StartEndTable

data FollowTableEntry =
  FollowTableEntry
  { followKey :: Regex
  , followVal :: [Regex] }
newtype FollowTable = FollowTable { followTableEntries :: [FollowTableEntry] }

instance Show FollowTableEntry where
  show entry@(FollowTableEntry key gotos) =
    showRegexType key ++ " | " ++ concat ((++ " ") <$> showRegexType <$> gotos)

instance Show FollowTable where
  show table@(FollowTable (x:xs)) = show x ++ "\n" ++ (show $ FollowTable xs)
  show table@(FollowTable []) = []

buildFollowTable :: StartEndTable -> FollowTable
buildFollowTable seTable@(StartEndTable entries) =
  let partialFollowTables = map seEntryToFollow entries
  in mergeTables partialFollowTables

seEntryToFollow :: StartEndTableEntry -> FollowTable
seEntryToFollow entry@(StartEndTableEntry rx@(RxAnd r1 r2) _ _ _) =
  let r1Last = lastRx r1
      r2First = firstRx r2
  in FollowTable $ map (\key -> FollowTableEntry key r2First) r1Last
seEntryToFollow entry@(StartEndTableEntry rx@(RxMany r) _ _ _) =
  let rLast = lastRx r
      rFirst = firstRx r
  in FollowTable $ map (\key -> FollowTableEntry key rFirst) rLast
seEntryToFollow entry@(StartEndTableEntry rx@(RxSome r) _ _ _) =
  let rLast = lastRx r
      rFirst = firstRx r
  in FollowTable $ map (\key -> FollowTableEntry key rFirst) rLast
seEntryToFollow _ = FollowTable []

mergeTables :: [FollowTable] -> FollowTable
mergeTables tables =
  let entriesByKey = groupedEntriesByKey $ allEntries tables
      entryResults = catMaybes $ map mergeEntriesOfOneKey entriesByKey
  in FollowTable entryResults

allEntries :: [FollowTable] -> [FollowTableEntry]
allEntries tables = concat $ map (\table@(FollowTable e) -> e) tables

groupedEntriesByKey :: [FollowTableEntry] -> [[FollowTableEntry]]
groupedEntriesByKey entries =
  let groupFunc (FollowTableEntry l _) (FollowTableEntry r _) = l == r
  in filter (not . null) $ groupBy groupFunc entries

mergeEntriesOfOneKey :: [FollowTableEntry] -> Maybe FollowTableEntry
mergeEntriesOfOneKey [] = Nothing
mergeEntriesOfOneKey entries =
  let (FollowTableEntry key _) = head entries
      gotos = nub $ concat $ map (\(FollowTableEntry _ g) -> g) entries
  in Just $ FollowTableEntry key gotos

searchByKey :: FollowTable -> Regex -> Maybe FollowTableEntry
searchByKey (FollowTable followTableEntries) regex =
  let searchEntries [] _ = Nothing
      searchEntries (entry@(FollowTableEntry key _):rest) regex =
        if key == regex then Just entry else searchEntries rest regex
  in searchEntries followTableEntries regex
