{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import qualified Control.Exception              as E
import qualified Data.Map                       as M
import           Test.Framework                 (defaultMain, testGroup, Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertEqual, assertFailure)

import           Data.Map.Syntax
import           Data.Map.Syntax.Tests
------------------------------------------------------------------------------
tests :: [Test]
tests = [testGroup "Simple insertion testing" insTests
        ,testGroup "Nested block insertion testing" nestingTests]


  

------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

