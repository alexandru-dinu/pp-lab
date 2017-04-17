data Colors = Red | Green | Blue

instance Eq Colors where
    (==) Red Red = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) _ _ = False



-- with monomorphic lists, things are simple
-- because we have no restrictions with Integer type (Eq, Show, Ord)
data IList = IEmpty | ICons Integer IList

l1 = ICons 1 (ICons 2 (ICons 3 (ICons 4 (ICons 5 IEmpty))))
l2 = ICons 1 (ICons 2 (ICons 3 (ICons 4 (ICons 6 IEmpty))))

showIList :: IList -> String
showIList IEmpty = "[]"
showIList (ICons h t) = show h ++ ":" ++ showIList t

instance Eq IList where
    (==) IEmpty IEmpty = True
    (==) (ICons h1 t1) (ICons h2 t2) = (h1 == h2) && ((==) t1 t2)

instance Show IList where
    show = showIList

instance Ord IList where
    (<=) IEmpty IEmpty = True
    (<=) (ICons h1 t1) (ICons h2 t2) = (h1 <= h2) && ((<=) t1 t2) 




-- ADT for natural numbers
data Natural = Zero | Succ Natural

showNatural :: Natural -> String
showNatural Zero = "0"
showNatural (Succ x) = "$" ++ showNatural x

n0 = Zero
n1 = Succ n0
n2 = Succ n1
n3 = Succ n2
n4 = Succ n3
n5 = Succ n4

-- default type-class membership
instance Show Natural where
    show = showNatural

{-
-- we need a mechanism for specifying showable types
data Showable a = SNatural Natural | SIList IList | SList (List a)
--data Showable a = SNatural Natural | SList (List (Showable a))

sshow :: (Showable a) -> String
sshow (SNatural n) = showNatural n
sshow (SIList il) = showIList il
sshow (SList l) = showLista l

showLista :: (List (Showable a)) -> String
showLista Empty = "[]"
showLista (Cons h t) = (sshow h) ++ ":" ++ (showLista t)

to extend this code, i.e. to define a new showable type, the programmer must:
    - add a new data constructor in Showable
    - if the new data is polymorphic, Showable itself must be modified (as in pairs)
    - in the latter case, the programmer must EXPLICITLY PACK values as showables. Example:

    one = Succ Zero
    Cons (SNat one) (Cons SNat Zero Void) 
    instead of:
    Cons one (Cons Zero Void)

    this particular aspect is quite annoying. Haskell provides a built-in alternative, which:
    * preserves the same principle as above
    * is much easier to use
-}


-- with polymorphic lists we have to pay extra attention to the type parameter a
data List a = Empty | Cons a (List a)

l3 = Cons 'a' (Cons 'b' (Cons 'c' (Cons 'd' (Cons 'e' Empty))))
l4 = Cons n0 (Cons n1 (Cons n2 (Cons n3 (Cons n4 (Cons n5 Empty)))))
l5 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 6 Empty))))

-- default membership
instance (Show a) => Show (List a) where
    show Empty = "[]"
    show (Cons h t) = (show h) ++ ":" ++ (show t)

-- defining a custom type-class
class MShowable a where
    -- minimal complete definition
    -- implementing this function
    mshow :: a -> String


-- custom type-class membership
instance MShowable Natural where
    mshow = showNatural

instance MShowable IList where
    mshow = showIList




-- -- this construction can be used for custom types (MShowable)
-- -- List Natural (for example)
-- instance (MShowable a) => MShowable (List a) where
--     mshow = mshowLista

-- mshowLista :: (MShowable a) => List a -> String
-- mshowLista Empty = "{}"
-- mshowLista (Cons h t) = (mshow h) ++ ":" ++ (mshowLista t)

-- this construction can be used for default types (Show)
instance (Show a) => MShowable (List a) where
    mshow = mshowLista

mshowLista :: (Show a) => List a -> String
mshowLista Empty = "{}"
mshowLista (Cons h t) = (show h) ++ ":" ++ (mshowLista t)



listMap :: (a -> b) -> (List a) -> (List b)
listMap f = fold ((Cons).f) Empty


    
-- fold
-- t should have kind '* -> *'
class Fold t where
    fold :: (a -> b -> b) -> b -> t a -> b

    -- this means that if we implement fold
    -- fsum comes for free
    fsum :: (Num a) => t a -> a
    fsum = fold (+) 0


instance Fold List where
    fold op acc Empty = acc
    fold op acc (Cons h t) = h `op` fold op acc t  

-- map
-- t should have kind '* -> *'
class Mappable t where
    mmap :: (a -> b) -> (t a) -> (t b)

instance Mappable List where
    mmap = listMap


{- Haskell classes:

   Show: show
   Ord: > , >=, etc.
   Eq: ==
   Num: +  
   Foldable: foldr
   Functor: map (generalization of map)
-}
