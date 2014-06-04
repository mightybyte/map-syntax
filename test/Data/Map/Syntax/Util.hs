{-# LANGUAGE ScopedTypeVariables #-}

module Data.Map.Syntax.Util where

------------------------------------------------------------------------------
import qualified Control.Exception              as E
import qualified Data.Map                       as M
import           Test.HUnit                     (assertFailure)

import           Data.Map.Syntax

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

dups :: (Eq a,Ord a) => [a] -> [a]
dups xs = let countMap = M.fromListWith (+) (zip xs $ repeat (1::Int))
          in  map fst . M.toList $ M.filter (>1) countMap
