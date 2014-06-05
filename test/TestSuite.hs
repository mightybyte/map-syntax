{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Test.Framework                 (defaultMain, testGroup, Test)
import           Data.Map.Syntax.Tests


------------------------------------------------------------------------------
tests :: [Test]
tests = [testGroup "Simple insertion testing" insTests
        ,testGroup "Monoid laws" monoidLaws
        ,testGroup "Nested block insertion testing" nestingTests]


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

