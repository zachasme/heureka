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
tautology x = not . Set.null $ complements x x




-- Computes the resolvent(s)
resolve :: Clause -> Clause -> Maybe Clause
resolve x y
  | Set.null c = Nothing
  | otherwise = Just $ Set.union (Set.difference x c) (Set.difference y $ deny c)
	where c = complements x y


--
resolveall :: Clause -> [Clause] -> [Clause]
resolveall x kb =
	filter (not . tautology)
	$ catMaybes $ map (resolve x) kb



parse :: String -> [Clause]
parse text = filter (not . Set.null) $ map parseline (map words $ lines text)

parseline :: [String] -> Clause
parseline ("--":_)  = Set.empty
parseline ("if":xs) = deny $ parseline xs
parseline (x:xs)    = Set.insert (Positive x) $ parseline xs
parseline _         = Set.empty





-- | attempts to prove the given conjecture from the given knowledge base
refutationproof :: Clause -> [Clause] -> Maybe ([Clause],Double)
refutationproof conjecture kb =
	Astar.search heuristic successors [origin] target
	where
		-- | refutation proofs start from the denial of the conjecture
		origin = deny conjecture
		-- | and we need to derive the empty set (a contradiction)
		target = Set.empty
		-- | the knowledge base is extended with the origin clause
		kb' = origin:kb
		-- | we encourage the search to use smaller clauses first
		-- by estimating the full cost from the clause size
		heuristic = fromIntegral . Set.size
		-- | anestor lol
		successors (x:xs)
			= map (\resolvent -> (resolvent, 1)) $ resolveall x $ nub $ kb'++xs


directproof conjecture kb =
	Astar.search heuristic successors origins target
	where
		-- | direct proofs start from the knowledge base clauses
		origins = kb
		-- | and we need to derive the conjecture
		target = conjecture
		-- | we encourage the search to use smaller clauses first
		-- by estimating the full cost from the clause size
		heuristic = fromIntegral . Set.size
		-- | anestor lol
		successors (x:xs)
			= map (\resolvent -> (resolvent, 1)) $ resolveall x $ nub $ kb++xs


main = do
	print "Refutation breakfast:"
	datafile <- readFile "../data/breakfast.txt"
	let kb = parse datafile
	let conjecture = Set.fromList [Positive "breakfast"]
	print $ refutationproof conjecture kb

	print "Refutation ancestortest:"
	datafile <- readFile "../data/ancestortest.txt"
	let kb = parse datafile
	let conjecture = Set.fromList [Positive "a", Positive "b"]
	let  Just (path,_) = refutationproof conjecture kb
	print $ reverse path

	print "Direct breakfast:"
	datafile <- readFile "../data/breakfast.txt"
	let kb = parse datafile
	let conjecture = Set.fromList [Positive "breakfast"]
	print $ directproof conjecture kb
