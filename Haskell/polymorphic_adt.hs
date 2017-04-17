-- polymorphic lists
data List a = Empty | Cons a (List a) deriving Show

list1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))
list2 = Cons 'a' (Cons 'b' (Cons 'c' (Cons 'd' (Cons 'e' Empty)))) 

swap = \f -> \x -> \y -> f y x

at :: Int -> List a -> a
at 0 (Cons x xs) = x
at i (Cons x xs) = at (i-1) xs

printList :: (Show a) => List a -> String
printList Empty = "[]"
printList (Cons x xs) = show x ++ ":" ++ (printList xs)

lfoldl :: (b -> a -> b) -> b -> List a -> b
lfoldl _ acc Empty = acc
lfoldl op acc (Cons x xs) = lfoldl op (op acc x) xs 

lfoldr :: (a -> b -> b) -> b -> List a -> b
lfoldr _ acc Empty = acc
lfoldr op acc (Cons x xs) = x `op` lfoldr op acc xs

lmap :: (a -> b) -> List a -> List b
lmap f = lfoldr ((Cons).f) Empty

lreverse :: List a -> List a
lreverse = lfoldl (swap Cons) Empty

lsize :: List a -> Int
lsize = lfoldl (\acc _ -> 1 + acc) 0

-- constructs a list of size min(size l1, size l2)
-- that has elements of new type c
lzipWith1 :: (a -> b -> c) -> List a -> List b -> List c
lzipWith1 op (Cons x1 xs1) (Cons x2 xs2) = Cons (op x1 x2) (lzipWith1 op xs1 xs2)
lzipWith1 _ _ _ = Empty

-- constructs a list of size max(size l1, size l2)
-- that can only have elements of the same type
lzipWith2 :: (a -> a -> a) -> List a -> List a -> List a
lzipWith2 _ Empty Empty = Empty
lzipWith2 _ l Empty = l
lzipWith2 _ Empty l = l
lzipWith2 op (Cons x1 xs1) (Cons x2 xs2) = Cons (op x1 x2) (lzipWith1 op xs1 xs2)

-- using foldr
zw op l1 l2 = let len = lsize l1 - 1 in 
                lfoldr (\f acc -> Cons (f (at (len - lsize acc) l2)) acc) Empty l1'
                where l1' = lmap (\x -> op x) l1





-- polymorphic trees
data Tree a = Null | Leaf a | Node a (Tree a) (Tree a) deriving Show

tree1 = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)
tree2 = Node 1 (Leaf 2) (Leaf 3)

{-
    1
   / \
  2  5
 / \
3  4
-}

-- tree traversals
-- preorder
rsd :: Tree a -> [a]
rsd Null = []
rsd (Leaf a) = [a]
rsd (Node key left right) = [key] ++ (rsd left) ++ (rsd right)

-- inorder
srd :: Tree a -> [a]
srd Null = []
srd (Leaf a) = [a]
srd (Node key left right) = (srd left) ++ [key] ++ (srd right)

-- postorder
sdr :: Tree a -> [a]
sdr Null = []
sdr (Leaf a) = [a]
sdr (Node key left right) = (sdr left) ++ (sdr right) ++ [key]


mirror :: Tree a -> Tree a
mirror Null = Null
mirror (Leaf a) = (Leaf a)
mirror (Node key left right) = Node key (mirror right) (mirror left)




-- accumulates starting from left
tfoldl :: (t -> t -> t) -> t -> (Tree t) -> t
tfoldl _ acc Null = acc
tfoldl op acc (Leaf a) = op acc a
tfoldl op acc (Node key left right) = let res = (tfoldl op acc left) in tfoldl op (op key res) right

-- accumulates starting from left
tfoldr :: (t -> t -> t) -> t -> (Tree t) -> t
tfoldr _ acc Null = acc
tfoldr op acc (Leaf a) = op acc a
tfoldr op acc (Node key left right) = let res = (tfoldr op acc right) in tfoldr op (op key res) left

-- accumulates going down and propagating up
tfold :: (t -> t -> t) -> t -> (Tree t) -> t
tfold _ acc Null = acc
tfold op acc (Leaf a) = op acc a
tfold op acc (Node key left right) = let rest = op onLeft onRight in op key rest
                                     where {onLeft = tfold op acc left;
                                            onRight = tfold op acc right;}




tmap :: (a -> b) -> (Tree a) -> (Tree b)
tmap _ Null = Null
tmap f (Leaf a) = Leaf (f a)
tmap f (Node key left right) = Node (f key) (tmap f left) (tmap f right) 

-- trees have the same structure
tzipWith :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
tzipWith op (Leaf a) (Leaf b) = Leaf (op a b)
tzipWith op (Node k1 l1 r1) (Node k2 l2 r2) = Node (op k1 k2) (tzipWith op l1 l2) (tzipWith op r1 r2)
tzipWith _ _ _ = Null
