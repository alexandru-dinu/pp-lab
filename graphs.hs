-- type aliases
type Node = Integer
type Edge = (Node, Node)

-- edge-list
type GraphL = [Edge]
-- adjacency-list
type GraphA = [(Node, [Node])]

-- representation 1 (undirected graph)
graph1 :: GraphL
graph1 = [(1, 2), (2, 3), (2, 4), (3, 4)]

-- representation 2 (directed graph)
-- adjacency lists
graph2 :: GraphA
graph2 = [(1, [2, 3]), (2, [3]), (3, [4]), (4, [2])]


cnvSingle :: (Node, [Node]) -> GraphL
cnvSingle (node, adj) = map (\x -> (node, x)) adj

cnv :: GraphA -> GraphL
cnv g = foldl (\acc e -> acc ++ (cnvSingle e)) [] g
-- e :: (Integer, [Integer])

-- utils
getNeighbours :: Node -> GraphA -> [Node]
getNeighbours _ [] = []
getNeighbours i (n:t) = if (i == fst n) then snd n else getNeighbours i t



-- given two nodes a and b in a graph, returns all the acyclic paths from a to b.

-- if the source is the same as the destination, then the path consists of that particular node
-- otherwise, add the source to the result and compute the rest of the path
-- the rest of the path is computed in the following manner:
-- for each edge in the edge list (graph), such that the first node (fst edge) of that edge is the current source (same as for each outgoing edge)
-- choose the path by recursively calling paths with the new source (snd edge), same destination and the new edge list
-- the new edge list contains all the edges from the initial edge-list, without the selected edge at the current step

paths :: Node -> Node -> GraphL -> [[Node]]
paths src dst edges
    | src == dst = [[src]]
    | otherwise = [
        src:path | edge <- edges, (fst edge) == src,
        path <- (paths (snd edge) dst [e | e <- edges, e /= edge])
    ]
