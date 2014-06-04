{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Map.Syntax.Tests where

------------------------------------------------------------------------------
import qualified Control.Exception as E
import Control.Monad
import qualified Data.List as L
import Data.Function (on)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertEqual, assertFailure)
import Test.QuickCheck (Arbitrary (arbitrary), Property, forAll)

import Data.Map.Syntax
import Data.Map.Syntax.Util


------------------------------------------------------------------------------
-- |Simple tests for not-nested maps
insTests :: [Test]
insTests =
  [testCase "Insert overwrite" overDup
  ,testCase "Insert over fail" failDup
  ,testCase "Reject duplicate" skipDup
  ,testProperty "Insert overwrite from list" prop_syntaxMatchesNubOver
  ,testProperty "Insert conditional from list" prop_syntaxMatchesNubCond
  ,testProperty "Insert error on dup from list" prop_syntaxMatchesNubErr]


------------------------------------------------------------------------------
-- |Simple tests of ##, #!, #?
overDup :: IO ()
overDup = assertEqual "Failed to overwrite duplicate entry"
          (runMapSyntax M.lookup M.insert m)
          (Right $ M.fromList [("firstName","Egon")])
  where m = do
          "firstName" ## "Peter"
          "firstName" ## "Egon"

failDup :: IO ()
failDup = assertEqual "Failed to overwrite duplicate entry"
          (runMapSyntax M.lookup M.insert m)
          (Left ["firstName"])
  where m = do
          "firstName" #! "Peter"
          "firstName" #! "Egon"

skipDup :: IO ()
skipDup = assertEqual "Failed to reject duplicate entry"
          (runMapSyntax M.lookup M.insert m)
          (Right $ M.fromList [("firstName","Peter")])
  where m = do
          "firstName" #? "Peter"
          "firstName" #? "Egon"

------------------------------------------------------------------------------
-- |Properties of ## (Overwrite with newest)
prop_syntaxMatchesNubOver :: [(String,Int)] -> Bool
prop_syntaxMatchesNubOver pairs = Right revNubMap == (toDataMap mSyntax)
  where mSyntax   = mapM_ (\(k,v) -> (k ## v)) pairs
        revNubMap = M.fromList . L.nubBy ((==) `on` fst) . L.reverse $ pairs
        -- Nub keeps the first of each unique entry, so reverse list to
        -- simulate keeping the last

prop_syntaxMatchesNubCond :: [(String,Int)] -> Bool
prop_syntaxMatchesNubCond pairs = Right nubMap == (toDataMap mSyntax)
  where mSyntax = mapM_ (\(k,v) -> (k #? v)) pairs 
        nubMap  = M.fromList . L.nubBy ((==) `on` fst) $ pairs

prop_syntaxMatchesNubErr :: [(String,Int)] -> Bool
prop_syntaxMatchesNubErr pairs =
  let mMap = toDataMap $ mapM_ (\(k,v) -> (k #! v)) pairs
--      keys    = map fst pairs
--      dupKeys = dups keys
  in if   pairs == L.nubBy ((==) `on` fst) pairs
     then mMap == (Right . M.fromList $ pairs)
     else case mMap of
       Right _ -> False  -- We expected (Left dupKeys)
       Left  _ -> True   -- Wasn't sure about semantics here
                         -- runMap ... ("a" #! 1) >> ("a" #! 2) >> ("a" #! 3)
                         -- should be (Left ["a"]), or (Left ["a","a"])?


------------------------------------------------------------------------------
-- |Tests for #! when do blocks are nested
nestingTests :: [Test]
nestingTests =
  [testCase "Nested error dups" nestedErr
  ,testCase "Nested error dups mapK" nestedErrMapK
  ,testCase "Nester error dups mapV" nestedErrMapV
  ,testCase "Nested overwrite dups" nestedOver]


nestedErr :: IO ()
nestedErr = assertEqual "Failed to error on duplicates across do blocks"
            (toDataMap bazErr)
            (Left ["haskell"])

nestedErrMapK :: IO ()
nestedErrMapK = assertEqual "Failed to error on mapK'ed dups across blocks"
                (toDataMap bazErrMapK)
                (Left ["haskella"])

nestedErrMapV :: IO ()
nestedErrMapV = assertEqual "Failed to error on mapV'ed dups across blocks"
                (toDataMap bazErrMapV)
                (Left ["haskell"])

fooErr :: MapSyntax String Int
fooErr = do
  "java" #! 4
  "haskell" #! 10

barErr :: MapSyntax String Int
barErr = do
  "python" #! 6
  "haskell" #! 12

bazErr :: MapSyntax String Int
bazErr = do
  fooErr
  barErr
  "extra" #! 1234

-- Exercise the partial list merging
bazErrMapK :: MapSyntax String Int
bazErrMapK = do
  mapK (++"a") fooErr
  mapK (++"a") barErr
  "extra" #! 1234

-- Exercise the partial list merging
-- (This is the test that Heist.SpliceAPI fails)
bazErrMapV :: MapSyntax String Int
bazErrMapV = do
  fooErr
  mapV (+1) barErr
  "extra" #! 1234

nestedOver :: IO ()
nestedOver = assertEqual "Failed to overwrite dup entries across blocks"
             (toDataMap bazOver)
             (Right $ M.fromList
              [("c",6),("haskell",12),("java",4),("extra",1234)])

fooOver :: MapSyntax String Int
fooOver = do
  "java" ## 4
  "haskell" ## 10

barOver :: MapSyntax String Int
barOver = do
  "c"  ## 6
  "haskell" ## 12

bazOver :: MapSyntax String Int
bazOver = do
  fooOver
  barOver
  "extra" ## 1234


