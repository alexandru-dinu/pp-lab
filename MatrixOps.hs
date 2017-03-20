-- lab 3

module MatrixOps where

--  matrix operations

--  define custom type aliases for a matrix
-- type Line = [Int]
-- type Column = [Int]
-- type Matrix = [Line]

--  define a test matrix
m = [[1,2,3], [4,5,6], [7,8,9]]

-- gets the i-th line: default list indexing
line i m = m !! i

--  gets the j-th column
--  construct a list that contains the j-th element in each sublist (line)
column j m = foldr (\line rest -> (line !! j):rest) [] m

--  returns m[i][j]
elemAt i j m = (line i m) !! j


--  map show on each element and then construct a string
getString l = ((foldr (\h t -> h ++ "\t" ++ t) "") . (map show)) l

--  constructs a string for each line and append a newline at the end
printMatrix m = putStr $ foldr (\line rest -> (getString line) ++ "\n" ++ rest) "" m


--	transpose
tr ([]:_) = [] -- done
tr m = (map head m):(tr (map tail m))


m1 = [[1, 2], [3, 4]]
m2 = [[5, 6], [7, 8]]

-- m1 + m2
msum m1 m2 = zipWith (zipWith (+)) m1 m2

--	multiplying m1 x m2
r i j = sum $ zipWith (*) i j
mult m1 m2 = map (\i -> (map (\j -> (r i j)) (tr m2))) m1
