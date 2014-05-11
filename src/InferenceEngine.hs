import Astar

import Data.List
import Data.Maybe
import Debug.Trace


data Literal
	= Positive String
    | Negative String
	deriving (Show, Ord, Eq)

type Clause = [Literal]

deny :: Literal -> Literal
deny (Positive x) = Negative x
deny (Negative x) = Positive x


-- 
resolve x y =
	case complement of
		Nothing -> Nothing
		Just complement ->
			Just $ (delete complement x) ++ (delete (deny complement) y)
	where
		complement = find (\literal -> deny literal `elem` y) x

--
resolveall x kb = nub $ catMaybes $ map (resolve x) kb




parse :: String -> [Clause]
parse text = map parseline (map words $ lines text)

parseline :: [String] -> Clause
parseline ("if":xs) = map deny $ parseline xs
parseline (x:xs) = (Positive x):parseline xs
parseline _ = []







datafilepath = "../data/breakfast.txt"


main = do
	datafile <- readFile datafilepath
	-- | compute knowledgebase
	let kb = parse datafile
	-- | assign hypthesis
	let hypothesis = Positive "breakfast"

	-- * REFUTATION-PROOF
	-- | origin is denial of hypothesis
	let origin = [deny hypothesis]
	-- | target is empty clause
	let target = []

	let heuristic x  = fromIntegral $ length x
	let successors x = map (\resolvent -> (resolvent, 1, resolvent)) $ resolveall x kb
	--print $ resolveall origin kb
	print $ Astar.search heuristic successors origin target