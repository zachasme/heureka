module PQueue where

import Data.Set (Set)
import qualified Data.Set as Set

empty = Set.empty
null = Set.null

singleton p x = Set.singleton (p,x)

push p x = Set.insert (p,x)
pop t = (snd min, Set.delete min t)
  where min = foldr1 (\(q,y) (p,x) -> if p > q then (p,x) else (q,y)) $ Set.toList t

find f t = Set.findMin $ Set.filter (\(p,x) -> f x) t