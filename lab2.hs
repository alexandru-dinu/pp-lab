-- Int will overflow, Integer will not

--fact :: Int -> Int -> Int
fact :: Integer -> Integer -> Integer
fact acc 0 = acc
fact acc n = fact (n * acc) (n - 1)


mZipWith f l [] = l
mZipWith f [] l = l
mZipWith f (e1:l1) (e2:l2) = (f e1 e2):(mZipWith f l1 l2)


-- [1,2,3] -> [[1], [1,2], [1,2,3]]
f1 n l = if n > length l then [] else (take n l) : (f1 (n+1) l)


-- function composition
-- f o g = h

--f :: b -> c
--g :: a -> b
h :: (b -> c) -> (a -> b) -> a -> c
h f g x = f (g x)
-- h (+1) (**2) 3 = 10.0

-- we can use the dot (.) operator
-- (.) f g x = f (g x)
-- (.) (+1) (**2) 3 = 10.0

-- complex example
-- remove all odd numbers and increment the even numbers
f l = (.) (map (+1)) (filter even) l

-- be careful with types
-- (^) and (**) both represent pow functions

-- (^) :: (Integral b, Num a) => a -> b -> a
-- (^) x y means x raised to the y power, where y is an Integer
--
-- (**) :: Floating a => a -> a -> a

-- even :: Integral a => a -> Bool

-- will throw an error
-- because (**) works with Floating type
-- while (even) works with Integral type 
-- p1 = (.) (map (**2)) (filter even)

--  will work fine
--  becuase (^) works with Integral type
p2 = (.) (map (^2)) (filter even)


-- merge sort
merge [] [] = []
merge [] l = l
merge l [] = l
merge (e1:l1) (e2:l2) = if (e1 < e2) then e1:(merge l1 (e2:l2)) else e2:(merge (e1:l1) l2)

mergesort [] = []
mergesort (e:[]) = (e:[])
mergesort l = let mid = (div (length l) 2) in merge (mergesort (take mid l)) (mergesort (drop mid l))
