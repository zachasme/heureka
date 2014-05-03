module City
( parse
, successors
, distance
, 
) where



parse file = "wat"

parseTokens [x1,y1,road,x2,y2] = road
parseTokens _ = "wat"

parseLine line = parseTokens $ words line



main = print $ parseLine "hej hej vej hej ej"



successors = 1