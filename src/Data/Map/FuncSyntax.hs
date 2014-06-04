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

module Data.Map.FuncSyntax where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State (State, MonadState, execState, modify, put)
import           Data.Monoid
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Strategy to use for duplicates
--
-- If we use functions here, then it's not possible to implement mapK and mapV
type DupStrat k v = k -> v -> v -> Either k v


------------------------------------------------------------------------------
-- | Representation of an indivdual item in a map
data ItemRep k v = ItemRep
    { irStrat :: DupStrat k v
    , irKey :: k
    , irVal :: v
    }


------------------------------------------------------------------------------
type MapRep k v = [ItemRep k v]


------------------------------------------------------------------------------
-- | A monad providing convenient syntax for defining maps.
newtype MapSyntaxM k v a = MapSyntaxM { unMapSyntax :: State (MapRep k v) a }
  deriving (Functor, Applicative, Monad, MonadState (MapRep k v))


------------------------------------------------------------------------------
-- | Monoid instance does a union of the two maps with the second map
-- overwriting any duplicates.
instance Monoid (MapSyntax k v) where
  mempty = return ()
  mappend = (>>)


------------------------------------------------------------------------------
-- | Convenient type alias that will probably be used most of the time.
type MapSyntax k v = MapSyntaxM k v ()


replaceDup :: DupStrat k v
replaceDup _ _ v2 = Right v2

ignoreDup :: DupStrat k v
ignoreDup _ v1 _ = Right v1

errDup :: DupStrat k v
errDup k _ _ = Left k


addWith :: DupStrat k v -> k -> v -> MapSyntax k v
addWith strat k v = modify (++ [ItemRep strat k v])


------------------------------------------------------------------------------
-- | Forces an entry to be added.  If the key already exists, its value is
-- overwritten.
(##) :: k -> v -> MapSyntax k v
(##) = addWith replaceDup
infixr 0 ##


------------------------------------------------------------------------------
-- | Tries to add an entry, but if the key already exists, then it throws an
-- error message.  This may be useful if name collisions are bad and you want
-- to crash when they occur.
(#!) :: k -> v -> MapSyntax k v
(#!) = addWith errDup
infixr 0 #!


------------------------------------------------------------------------------
-- | Inserts into the map only if the key does not already exist.
(#?) :: k -> v -> MapSyntax k v
(#?) = addWith ignoreDup
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
        Just v1 -> replace accum v1 ir
        Nothing -> (es, forceIns irKey irVal m)

    replace (es,m) v1 ItemRep{..} =
      case irStrat irKey v1 irVal of
        Left k -> (es++[k], m)
        Right v -> (es, forceIns irKey v m)


------------------------------------------------------------------------------
execMapSyntax :: MapSyntaxM k v a -> MapRep k v
execMapSyntax ms = execState (unMapSyntax ms) mempty


-- Can't write these because DupStrat is contravariant

------------------------------------------------------------------------------
-- | Maps a function over all the keys.
--mapK :: (k1 -> k2) -> MapSyntaxM k1 v a -> MapSyntax k2 v
--mapK f = MapSyntaxM . put .
--    map (\ir -> ir { irKey = f (irKey ir) }) . execMapSyntax
--  where
--    g ir = ir { irKey = f (irKey ir)
--              , irStrat = h }
--      where
--        h k v2 v2 = irStrat (f k) v1 v2


------------------------------------------------------------------------------
-- | Maps a function over all the values.
--mapV :: (v1 -> v2) -> MapSyntaxM k v1 a -> MapSyntax k v2
--mapV f = MapSyntaxM . put .
--    map (\ir -> ir { irVal = f (irVal ir) }) . execMapSyntax


