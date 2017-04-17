-- ad-hoc polymorphism is implemented in Haskell via type-classes
{-
Ad-hoc polymorphism refers to when a value is able to adopt any one of several types 
because it, or a value it uses, has been given a separate definition for each of those types.

You can recognise the presence of ad-hoc polymorphism by looking for constrained type variables: 
that is, variables that appear to the left of =>
-}
data Naturals = Zero | Succ Naturals

instance Show Naturals where
	show = show . convert where 
			convert Zero = 0;
			convert (Succ n) = 1 + convert n
{-
the type of show . convert is Naturals → String, since convert :: Naturals → Integer. 
This example also shows ad-hoc polymorphism in action. 
In the above expression (the functional composition), 
the general type show :: (Show a) ⇒ a → String of show becomes via unification ::Integer → String. 
Thus, the compiler knows to call the integer implementation of show, which is part of Prelude.
-}

-- another example:
data List a = Empty | Cons a (List a)

instance (Show a) => Show (List a) where
	show Empty = "[]"
	show (Cons h t) = show h ++ ":" ++ show t
	-- here, show h is from Prelude (where h can be h :: Integer, h :: Char, h :: String etc)
	-- and show t is the recursive call

-- parametric polymorphism
{-
Parametric polymorphism refers to when the type of a value 
contains one or more (unconstrained) type variables, 
so that the value may adopt any type that results 
from substituting those variables with concrete types.
-}

-- simplest example
identity :: a -> a
identity x = x

{-
The above function contains an unconstrained type variable a in its type, 
and so can be used in a context requiring 
Char -> Char or 
Integer -> Integer or 
(Bool -> Maybe Bool) -> (Bool -> Maybe Bool) 
or any of a literally infinite list of other possibilities.
-}



convert :: (List a) -> [a]
convert Empty = []
convert (Cons h t) = h:convert t

-- more info: https://wiki.haskell.org/Polymorphism#Parametric_polymorphism