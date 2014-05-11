module Clauses
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data Literal a = Proposition a
               | Denial (Literal a)

type Clause a = [Literal a]


parse text = foldl parselol [] (map words $ lines text)

parselol x = x



--main = print $ [1]

datafilepath = "../data/pidgeon.txt"


main = do
	datafile <- readFile datafilepath
	let clauses         = Clauses.parse datafile
	print $ clauses