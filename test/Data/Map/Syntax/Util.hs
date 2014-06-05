{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Map.Syntax.Util where

------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import qualified Control.Exception              as E
import           Control.Monad
import           Control.Monad.State
import qualified Data.Map                       as M
import           Test.QuickCheck                (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen
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


------------------------------------------------------------------------------
--instance (Arbitrary a, Arbitrary b) => Arbitrary (MapSyntax a b) where
instance Arbitrary (MapSyntax String Int) where
  arbitrary = do
    items <- arbitrary
    let i = items :: [ItemRep String Int]
    return $ MapSyntaxM {unMapSyntax = return items}
{-
    un <- foldM (\acc (ItemRep strat k v) -> (acc >> insertOp strat k v)) (return ()) items
    let a = un :: Gen ()
    return un
      where insertOp :: DupStrat -> String -> Int -> MapSyntax String Int
            insertOp Replace k v = (k ## v)
            insertOp Ignore  k v = (k #? v)
            insertOp Error   k v = (k #! v)
-}

instance (Arbitrary a, Arbitrary b) => Arbitrary (ItemRep a b) where
  arbitrary = oneof [liftM2 (ItemRep Replace) arbitrary arbitrary
                    ,liftM2 (ItemRep Ignore)  arbitrary arbitrary
                    ,liftM2 (ItemRep Error)   arbitrary arbitrary
                    ]
