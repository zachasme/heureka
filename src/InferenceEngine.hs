import Astar

import Debug.Trace

datafilepath = "../data/pidgeon.txt"


data Literal = Proposition String
             | Denial (Literal)
	deriving (Show, Ord, Eq)

type Clause = [Literal]


parse :: String -> [Clause]
parse text = map parseline (map words $ lines text)

parseline :: [String] -> Clause
parseline ("or":xs) = parseline xs
parseline ("non":x:xs) = (Denial (Proposition x)):parseline xs
parseline (x:xs) = (Proposition x ): parseline xs
parseline _ = []



-- direct distance between two nodes
distance x = length x


main = do
	datafile <- readFile datafilepath
	-- | compute knowledgebase
	let kb = parse datafile
	-- | assign hypthesis
	let hypothesis = Proposition "wat"

	-- * REFUTATION-PROOF
	-- | origin is denial of hypothesis
	let origin = [Denial hypothesis]
	-- | target is empty clause
	let target = []

	let successors x = [([Proposition "arg"], 4, "lol")]
	let heuristic x  = 1
	print $ Astar.search heuristic successors origin target