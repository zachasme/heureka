module PriorityMap where

import Data.Map (Map)
import qualified Data.Map as Map

empty = Map.empty
null  = Map.null

singleton k p x = Map.singleton k (p,x)
insert    k p x = Map.insert    k (p,x)

pop t           = (snd min, Map.delete k t)
  where (k,min) = foldr1 (\(pmin,xmin) (p,x) -> if p < pmin then (p,x) else (pmin,xmin)) $ Map.toList t

find f t = Map.findMin $ Map.filter (\(p,x) -> f x) t