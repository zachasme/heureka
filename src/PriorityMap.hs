module PriorityMap where

import Data.Map (Map)
import qualified Data.Map as Map

empty = Map.empty
null  = Map.null

singleton p k x = Map.singleton k (p,x)
insert    p k x = Map.insert    k (p,x)

lookup k t =
	case Map.lookup k t of
		Just (p,x) -> Just x
		Nothing    -> Nothing

pop t           = ((k, xmin), Map.delete k t)
  where (k,(pmin,xmin)) = foldr1 (\(kmin,(pmin,xmin)) (k,(p,x)) -> if p < pmin then (k,(p,x)) else (kmin,(pmin,xmin))) $ Map.toList t