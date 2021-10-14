-- lab 4
-- TDA-uri monomorfice: datele contin elemente de un singur tip
-- List Integer / Tree Integer
-- data <type> = [<constructors>]
-- NATURALS
{-
Nat = tip nou (abstract) de date
dupa = -> constructorii de baza ai tipului
Zero = constructor nular
Succ = constructor intern : ia o valoare de tip Nat si construieste o valoare de tip Nat

privite ca functii:
Zero :: Nat
Succ :: Nat -> Nat

-}
data Nat
  = Zero
  | Succ Nat
  deriving (Show)

madd :: Nat -> Nat -> Nat
madd Zero expr = expr
madd (Succ x) y = Succ (madd x y)

minc :: Nat -> Nat
minc x = (Succ x)

-- type alias
type Conversion = Nat -> Integer

convertToInt :: Conversion
convertToInt Zero = 0
convertToInt (Succ x) = 1 + convertToInt x

-- data type has same name as external constructor
data Student =
  Student String Integer

-- s = Student Alex 10
-- s :: Student
-- BOOLEAN LOGIC & FORMULAS
-- True, False: constructori nulari
data BBool
  = BTrue
  | BFalse
  deriving (Show)

mnot :: BBool -> BBool
mnot BTrue = BFalse
mnot BFalse = BTrue

mand :: BBool -> BBool -> BBool
mand BTrue BTrue = BTrue
mand _ _ = BFalse

mor :: BBool -> BBool -> BBool
mor BFalse BFalse = BFalse
mor _ _ = BTrue

data Formula
  = Var String
  | And Formula Formula
  | Or Formula Formula
  | Not Formula
  deriving (Show)

showFormula :: Formula -> String
showFormula (Var s) = s
showFormula (And f1 f2) =
  "(" ++ showFormula f1 ++ " ^ " ++ showFormula f2 ++ ")"
showFormula (Or f1 f2) = "(" ++ showFormula f1 ++ " V " ++ showFormula f2 ++ ")"
showFormula (Not f) = "~(" ++ showFormula f ++ ")"

type Interpretation = String -> BBool

inter "x" = BTrue
inter "y" = BFalse

--etc
evalFormula :: Formula -> BBool
evalFormula (Var x) = inter x
evalFormula (And f1 f2) = mand (evalFormula f1) (evalFormula f2)
evalFormula (Or f1 f2) = mor (evalFormula f1) (evalFormula f2)
evalFormula (Not f) = mnot (evalFormula f)

-- TREES
data Tree
  = Null
  | Node Integer Tree Tree
  | Leaf Integer
  deriving (Show)

{-
Null :: Tree (nular)
Node :: Integer -> Tree -> Tree -> Tree (intern)
Leaf :: Integer -> Tree (extern)
-}
t = Node 1 (Leaf 2) (Leaf 3)

treeSum :: Tree -> Integer
treeSum Null = 0
treeSum (Leaf x) = x
treeSum (Node r left right) = r + treeSum left + treeSum right

-- foldTree :: (Integer -> b -> b) -> b -> Tree -> b
foldTree op acc Null = acc
-- foldTree op acc (Node x Null Null) = op acc x 
foldTree op acc (Leaf x) = op acc x -- Leaf
foldTree op acc (Node key left right) =
  let val = foldTree op acc left
   in foldTree op (op key val) right

treeSum' = foldTree (\key l -> key + l) 0

-- LISTS
data List
  = Empty
  | Cons Integer List
  deriving (Show)

-- instance Show List where
--     show Empty = "[]"
--     show (Cons h l) = show h ++ ":" ++ show l
list = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))

-- converts our List into Haskell's default list type
convert :: List -> [Integer]
convert Empty = []
convert (Cons h t) = h : (convert t)

-- data Maybe a = Nothing | Just a
-- not useful on "Nothing"
extract :: Maybe a -> a
extract (Just a) = a

mcons :: Integer -> List -> List
mcons x l = Cons x l

mhead :: List -> Maybe Integer
mhead Empty = Nothing
mhead (Cons h t) = Just h

head' = (extract . mhead)

mtail :: List -> List
mtail Empty = Empty
mtail (Cons h t) = t

minit_aux :: List -> List
minit_aux (Cons x Empty) = Empty
minit_aux (Cons h t) = mcons h (minit_aux t)

-- wrapper that handles the Empty list case => Nothing
minit :: List -> Maybe List
minit Empty = Nothing
minit l = Just (minit_aux l)

mlast_aux :: List -> Integer
mlast_aux (Cons x Empty) = x
mlast_aux (Cons h t) = mlast_aux t

-- wrapper that handles the Empty list case => Nothing
mlast :: List -> Maybe Integer
mlast Empty = Nothing
mlast l = Just (mlast_aux l)

mtake :: Int -> List -> List
mtake 0 l = Empty
mtake i Empty = Empty
mtake i (Cons h t) = mcons h (mtake (i - 1) t)

mdrop :: Int -> List -> List
mdrop 0 l = l
mdrop i Empty = Empty
mdrop i (Cons h t) = mdrop (i - 1) t

-- higher order functions
mfoldr :: (Integer -> b -> b) -> b -> List -> b
mfoldr op init Empty = init
mfoldr op init (Cons h t) = h `op` (mfoldr op init t)

mfoldl :: (b -> Integer -> b) -> b -> List -> b
mfoldl op init Empty = init
mfoldl op init (Cons h t) = mfoldl op (op init h) t

mmap :: (Integer -> Integer) -> List -> List
mmap f l = mfoldr (\h t -> mcons (f h) t) Empty l

mzipWith :: (Integer -> Integer -> Integer) -> List -> List -> List
mzipWith op Empty Empty = Empty
mzipWith op l Empty = l
mzipWith op Empty l = l
mzipWith op (Cons h1 t1) (Cons h2 t2) = mcons (op h1 h2) (mzipWith op t1 t2)

mreverse :: List -> List
mreverse l = mfoldl (\x y -> mcons y x) Empty l
