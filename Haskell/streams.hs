import Data.List

--	constructs an infinite list using 'next' rule and starting from 'init'
build :: (a -> a) -> a -> [a]
build next init = init:(build next (next init))

-- constructs an infinite list using 'next' operator and starting from 2 'inits'
build' :: (a -> a -> a) -> a -> a -> [a]
build' next i1 i2 = i1:i2:(build' next (next i1 i2) (next i2 (next i1 i2)))

-- "infinite" list containing ones
ones = build (\x -> 1) 1

-- "all" natural numbers
naturals = build (\x -> x+1) 0

-- "all" odd natural numbers
odds = map (\x -> 2 * x + 1) naturals
-- same thing, but directly using build
odds' = build (+ 2) 1

-- two ways of generating "all" Fibonacci numberss
fibn = build' (+) 0 1
fibn' = 0:1:zipWith (+) fibn' (tail fibn')

--	sieve of Eratosthenes
sieve (h:t) = h:(sieve (filter (\x -> x`mod`h /= 0) t))
primes = sieve (drop 2 naturals)
