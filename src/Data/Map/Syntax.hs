{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-|

An API implementing a convenient syntax for defining maps.  This module was
born from the observation that a list of tuples is semantically ambiguous
about how duplicate keys should be handled.  Additionally, the syntax is
inherently rather cumbersome and difficult to work with.  This API takes
advantage of do notation to provide a very light syntax for defining maps
while at the same time eliminating the semantic ambiguity of alists.

Here's an example:

> foo :: MapSyntax Text
> foo = do
>   "firstName" ## "John"
>   "lastName"  ## "Smith"

-}

module Data.Map.Syntax
  ( DupStrat(..)
  , ItemRep(..)
  , MapSyntaxM
  , MapSyntax
  , add
  , add'
  , (##)
  , (#!)
  , (#?)
  , runMapSyntax
  , mapK
  , mapV
  ) where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
import           Data.Monoid
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Strategy to use for duplicates
data DupStrat = Replace | Ignore | Error


------------------------------------------------------------------------------
-- | Representation of an indivdual item in a map
data ItemRep k v = ItemRep
    { irStrat :: DupStrat
    , irKey :: k
    , irVal :: v
    }


------------------------------------------------------------------------------
type MapRep k v = [ItemRep k v]


------------------------------------------------------------------------------
-- | A monad providing convenient syntax for defining maps.
newtype MapSyntaxM k v a = MapSyntaxM { unMapSyntax :: State (MapRep k v) a }
  deriving (Functor, Applicative, Monad)


------------------------------------------------------------------------------
-- | Monoid instance does a union of the two maps with the second map
-- overwriting any duplicates.
instance Monoid (MapSyntax k v) where
  mempty = return ()
  mappend = (>>)


------------------------------------------------------------------------------
-- | Convenient type alias that will probably be used most of the time.
type MapSyntax k v = MapSyntaxM k v ()


------------------------------------------------------------------------------
add :: DupStrat -> k -> v -> MapSyntax k v
add strat k v = add' [ItemRep strat k v]


------------------------------------------------------------------------------
add' :: [ItemRep k v] -> MapSyntax k v
add' irs = MapSyntaxM $ modify (++ irs)


------------------------------------------------------------------------------
-- | Forces an entry to be added.  If the key already exists, its value is
-- overwritten.
(##) :: k -> v -> MapSyntax k v
(##) = add Replace
infixr 0 ##


------------------------------------------------------------------------------
-- | Tries to add an entry, but if the key already exists, then it throws an
-- error message.  This may be useful if name collisions are bad and you want
-- to crash when they occur.
(#!) :: k -> v -> MapSyntax k v
(#!) = add Error
infixr 0 #!


------------------------------------------------------------------------------
-- | Inserts into the map only if the key does not already exist.
(#?) :: k -> v -> MapSyntax k v
(#?) = add Ignore
infixr 0 #?


------------------------------------------------------------------------------
-- | Runs the MapSyntaxM monad to generate a map.
runMapSyntax
    :: (Monoid map)
    => (k -> map -> Maybe v)
    -- ^ Function that tests whether the key exists in the map
    -> (k -> v -> map -> map)
    -- ^ Function to force-insert a key-value pair into the map
    -> MapSyntaxM k v a
    -> Either [k] map
runMapSyntax lookupEntry forceIns ms =
    case res of
      ([],m) -> Right m
      (es,_) -> Left es
  where
    res = foldl f (mempty, mempty) $ execState (unMapSyntax ms) mempty
    f accum@(es,m) ir@ItemRep{..} =
      case lookupEntry irKey m of
        Just v1 -> replace accum ir
        Nothing -> (es, forceIns irKey irVal m)

    replace (es,m) ItemRep{..} =
      case irStrat of
        Replace -> (es, forceIns irKey irVal m)
        Ignore -> (es, m)
        Error -> (es ++ [irKey], m)


------------------------------------------------------------------------------
execMapSyntax :: MapSyntaxM k v a -> MapRep k v
execMapSyntax ms = execState (unMapSyntax ms) mempty


------------------------------------------------------------------------------
-- | Maps a function over all the keys.
mapK :: (k1 -> k2) -> MapSyntaxM k1 v a -> MapSyntax k2 v
mapK f ms = add' items
  where
    items = map (\ir -> ir { irKey = f (irKey ir) }) $ execMapSyntax ms


------------------------------------------------------------------------------
-- | Maps a function over all the values.
mapV :: (v1 -> v2) -> MapSyntaxM k v1 a -> MapSyntax k v2
mapV f ms = add' items
  where
    items = map (\ir -> ir { irVal = f (irVal ir) }) $ execMapSyntax ms


