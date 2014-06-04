{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import qualified Control.Exception              as E
import qualified Data.Map                       as M
import           Test.Framework                 (defaultMain, testGroup, Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertEqual, assertFailure)

import           Data.Map.Syntax

------------------------------------------------------------------------------
tests :: [Test]
tests = [testGroup "Quick failure from overlapping insert" insTests
        ,testGroup "Fail nested duplication" nestingTests]


------------------------------------------------------------------------------
-- |Simple tests of ##, #!, #?
insTests :: [Test]
insTests = [testCase "Insert overwrite" overDup
           ,testCase "Insert over fail" failDup
           ,testCase "Reject duplicate" skipDup]

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
-- |Tests for #! when do blocks are nested
nestingTests :: [Test]
nestingTests = [testCase "Simple nested insertion failure" nestedInsFail
               ,testCase "Simple nested insertion failure - try to print"
                nestedInsFailUsage
               ,testCase "Nested duplicate entry overwrite failed" nestedDup]

-- |This test fails (no exception)
nestedInsFail :: IO ()
nestedInsFail = 
  expectException . return . toDataMap $ baz

-- |This test passes
nestedInsFailUsage :: IO ()
nestedInsFailUsage =
  expectException . print . toDataMap $ baz

foo :: MapSyntax String Int
foo = do
  "java" #! 4
  "haskell" #! 12

bar :: MapSyntax String Int
bar = do
  "python" #! 6
  "haskell" #! 11

baz :: MapSyntax String Int
baz = do
  foo
  bar
  "extra" #! 1234

nestedDup :: IO ()
nestedDup = assertEqual "Failed to overwrite nested do block duplicate"
            (toDataMap bazOver)
            (Right $ M.fromList [("java",4),("haskell",11),("extra",1234),("c",6)])

fooOver :: MapSyntax String Int
fooOver = do
  "java" ## 4
  "haskell" ## 12

barOver :: MapSyntax String Int
barOver = do
  "c"  ## 6
  "haskell" ## 11

bazOver :: MapSyntax String Int
bazOver = do
  fooOver
  barOver
  "extra" ## 1234

  
------------------------------------------------------------------------------
-- |Utilities
expectException :: IO a -> IO ()
expectException m = do
  e <- E.try m
  case e of
    Left (z :: E.SomeException) -> length (show z) `seq` return ()
    Right _ -> assertFailure "Expected exception, didn't get it."
  
toDataMap :: (Show k, Ord k) => MapSyntax k v -> Either [k] (M.Map k v)
toDataMap = runMapSyntax M.lookup M.insert


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

