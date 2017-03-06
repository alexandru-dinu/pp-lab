sum' [] = 0
sum' (e:l) = e + (sum l)

bsum' acc [] = acc
bsum' acc (e:l) = bsum' (acc+e) l
bsum l = bsum' 0 l 

-- naive
fact1 0 = 1
fact1 1 = 1
fact1 n = n * fact1 (n-1)

-- tail recursive
fact2 acc 1 = acc
fact2 acc n = fact2 (n*acc) (n-1)

fact n = fact2 1 n


-- naive
fib1 0 = 1
fib1 1 = 1
fib1 n = (fib1 (n-1)) + (fib1 (n-2))

-- tail recursive
fib2 a _ 0 = a
fib2 a b n = fib2 (a+b) a (n-1)

fib n = fib2 0 1 n




-- TDA / pattern matching
size :: [a] -> Int
size [] = 0
size (e:l) = 1 + (size l)

app :: [a] -> [a] -> [a]
app [] l = l
app (e1:l1) l2 = e1:(app l1 l2) 

rev :: [a] -> [a]
rev [] = []
rev (e:l) = (rev l) `app` (e:[])




-- higher order functions
-- anonymous functions (lambdas)
rev' l = foldl (\list el -> el:list) [] l

-- curry vs. uncurry
-- curry: op = \list -> \el -> (el:list)
-- se pot observa mai usor inchiderile functionale

-- uncurry: op = \list el -> (el:list)

-- map implemented with foldr
mymap f l = foldr (\el list -> (f el):list) [] l
-- filter implemented with foldr
myfilter p l = let op = (\el list -> if (p el) then (el:list) else list) in foldr op [] l

mfoldr op init [] = init
mfoldr op init (e:l) = e `op` (mfoldr op init l)

mfoldl op init [] = init
mfoldl op init (e:l) = mfoldl op (init `op` e) l

-- closures
p a b = a + b
q x = p 2 x

-- (+ 1) is a closure: function that waits another Num (x) and returns x + 1
r x = (+) 1 x





