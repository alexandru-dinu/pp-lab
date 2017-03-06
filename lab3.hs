--  operatii pe matrici

type Line = [Int]
type Column = [Line]

m = [[1,2,3], [4,5,6], [7,8,9]]

-- default list indexing
line i m = m !! i

-- construct a list that contains the j-th element in each sublist (line)
column j m = foldr (\line rest -> (line !! j):rest) [] m

elemAt i j m = (line i m) !! j


--	map show on each element and then construct a string
getString l = ((foldr (\h t -> h ++ "\t" ++ t) "") . (map show)) l

printMatrix m = putStr $ foldr (\line rest -> (getString line) ++ "\n" ++ rest) "" m




--	transpose
tr ([]:_) = []
tr m = (map head m):(tr (map tail m))


m1 = [[1, 2], [3, 4]]
m2 = [[5, 6], [7, 8]]

--	multiplying m1 x m2
r i j = sum $ zipWith (*) i j

mult m1 m2 = map (\i -> (map (\j -> (r i j)) (tr m2))) m1
