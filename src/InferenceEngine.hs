import Astar

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace


data Literal
	= Positive String
		| Negative String
	deriving (Show, Ord, Eq)

type Clause = Set Literal



-- Denies each literal in a given clause
deny :: Clause -> Clause
deny x = Set.map inner x
	where
		inner (Positive x) = Negative x
		inner (Negative x) = Positive x

-- Computes the complimentary literals between two clauses
-- returns them as they are in the first clause
complements :: Clause -> Clause -> Clause
complements x y = Set.intersection x $ deny y

-- A clause is a tautology if it contains complementary literals
tautology :: Clause -> Bool
tautology x = Set.null $ complements x x




-- Computes the resolvent(s)
resolve :: Clause -> Clause -> Maybe Clause
resolve x y
  | Set.null c = Nothing
  | otherwise = Just $ Set.union (Set.difference x c) (Set.difference y $ deny c)
	where c = complements x y


--
resolveall :: Clause -> [Clause] -> [Clause]
resolveall x kb = catMaybes $ map (resolve x) kb



parse :: String -> [Clause]
parse text = filter (not . Set.null) $ map parseline (map words $ lines text)

parseline :: [String] -> Clause
parseline ("--":_)  = Set.empty
parseline ("if":xs) = deny $ parseline xs
parseline (x:xs)    = Set.insert (Positive x) $ parseline xs
parseline _         = Set.empty






refutationproof :: Clause -> [Clause] -> Maybe ([Clause],Double)
refutationproof conjecture kb =
	Astar.search heuristic successors origin target
	where
		origin = deny conjecture
		target = Set.empty
		heuristic = fromIntegral . Set.size
		successors (x:xs)
			= map (\resolvent -> (resolvent, 1)) $ resolveall x $ nub $ kb++xs




main = do
	datafile <- readFile "../data/breakfast.txt"
	let kb = parse datafile
	let conjecture = Set.fromList [Positive "breakfast"]
	print $ refutationproof conjecture kb

	datafile <- readFile "../data/ancestortest.txt"
	let kb = parse datafile
	let conjecture = Set.fromList [Positive "a", Positive "b"]
	print $ refutationproof conjecture kb