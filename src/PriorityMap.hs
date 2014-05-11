-----------------------------------------------------------------------------
--
-- A wrapper around the standard map implementation that adds
-- a priority to each key-value pair such that the lowest
-- priority pair may be popped from the map
--
-----------------------------------------------------------------------------


module PriorityMap
	( null
	, empty
	, singleton
	, insert
	, PriorityMap.lookup
	, pop
) where

-- | hides standard library lookup and null functions (because we are using the same names)
import Prelude hiding (lookup,null)
-- | the priority map is a wrapper around the standard Map implementation
import qualified Data.Map as Map

type PMap k p a = Map.Map k (p,a)

-- | the empty p map
empty :: PMap k p a
empty = Map.empty

-- | is the priority map empty?
null :: PMap k p a -> Bool
null  = Map.null

-- | A priority map with single given value
singleton :: p -> k -> a -> PMap k p a
singleton p k x = Map.singleton k (p,x)

-- | Insert a new value at key k with priority p
insert :: Ord k => p -> k -> a -> PMap k p a -> PMap k p a
insert p k x = Map.insert k (p,x)

-- | Looks up a value by its key in the p map
lookup :: Ord k => k -> PMap k p a -> Maybe a
lookup k t =
	case Map.lookup k t of
		Just (p,x) -> Just x
		Nothing    -> Nothing

-- | pops the value with lowest priority
pop :: (Ord k, Ord p) => PMap k p a -> ((k,a), PMap k p a)
pop t = ((k, xmin), Map.delete k t)
  where (k,(pmin,xmin)) = foldr1 (\(kmin,(pmin,xmin)) (k,(p,x)) -> if p < pmin then (k,(p,x)) else (kmin,(pmin,xmin))) $ Map.toList t