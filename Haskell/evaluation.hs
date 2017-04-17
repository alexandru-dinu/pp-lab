import Data.List

-- proof that Haskell uses normal evaluation strategy
loop x = x:loop x
stop x = 1
ends x = stop $ loop x
-- stop will be called with the argument (loop x), and will just return 1
-- loop x will not be evaluated

-- another example
-- infinite list of Falses
fls = False:fls

-- will expand to (False && (...)) and
-- will stop because False && X = False
f1 = foldr (&&) True fls

-- will expand to ((True && False) && False) && ... and
-- will not stop because no reductions are made 
f2 = foldl (&&) True fls



-- evaluation applications: streams
-- due to Haskell's lazy evaluation we can take advantage of the concept of
-- "infinite lists"

--	constructs an infinite list using 'next' rule and starting from 'init'
build :: (a -> a) -> a -> [a]
build next init = init:(build next (next init))

-- constructs an infinite list using 'next' operator and starting from 2 'inits'
build' :: (a -> a -> a) -> a -> a -> [a]
build' next i1 i2 = i1:i2:(build' next (next i1 i2) (next i2 (next i1 i2)))

-- "infinite" list containing ones
ones = build (\x -> 1) 1


-- example: take 10 ones will return [1,1,1,1,1,1,1,1,1,1]

-- "all" natural numbers
naturals = build (\x -> x+1) 0

-- "all" odd natural numbers
odds = map (\x -> 2 * x + 1) naturals
-- same thing, but directly using build
odds' = build (+ 2) 1

-- two ways of generating "all" Fibonacci numbers
fibn = build' (+) 0 1
fibn' = 0:1:zipWith (+) fibn' (tail fibn')

--	sieve of Eratosthenes
sieve (h:t) = h:(sieve (filter (\x -> x`mod`h /= 0) t))
primes = sieve (drop 2 naturals)
