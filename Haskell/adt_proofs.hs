-- skel from: https://www.codewars.com/kata/599d973255342a0ce400009b
{-# LANGUAGE GADTs, DataKinds, TypeFamilies, UndecidableInstances
  #-}

-- | The natural numbers.
data Nat
  = Z
  | S Nat
  deriving (Show)

-- | The axioms of even numbers.
data Even (a :: Nat) :: *
    -- | Zero is even.
      where
  ZeroEven :: Even Z
    -- | If n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of odd numbers.
data Odd (a :: Nat) :: *
    -- | One is odd.
      where
  OneOdd :: Odd (S Z)
    -- | If n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family Add (n :: Nat) (m :: Nat) :: Nat

type instance Add Z m = m

type instance Add (S n) m = S (Add n m)

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

fromOdd :: Odd a -> Int
fromOdd OneOdd = 1
fromOdd (NextOdd n) = 2 + fromOdd n

fromEven :: Even a -> Int
fromEven ZeroEven = 0
fromEven (NextEven n) = 2 + fromEven n

zero = ZeroEven

one = OneOdd

two = NextEven zero

three = NextOdd one

four = NextEven two

five = NextOdd three

six = NextEven four

seven = NextOdd five

eight = NextEven six

nine = NextOdd seven

ten = NextEven eight

-- | Proves that if n is even, n+1 is odd.
-- Notice how I use the axioms here.
evenPlusOne :: Even n -> Odd (S n)
evenPlusOne ZeroEven = OneOdd
evenPlusOne (NextEven n) = NextOdd (evenPlusOne n)

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne OneOdd = NextEven ZeroEven
oddPlusOne (NextOdd n) = NextEven (oddPlusOne n)

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (Add n m)
evenPlusEven ZeroEven m = m
evenPlusEven (NextEven n) m = NextEven (evenPlusEven n m)

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (Add n m)
oddPlusOdd OneOdd OneOdd = NextEven ZeroEven
oddPlusOdd OneOdd (NextOdd m) = NextEven (oddPlusOdd OneOdd m)
oddPlusOdd (NextOdd n) m = NextEven (oddPlusOdd n m)

-- | Proves even + odd = odd
evenPlusOdd :: Even n -> Odd m -> Odd (Add n m)
evenPlusOdd ZeroEven OneOdd = OneOdd
evenPlusOdd ZeroEven m = m
evenPlusOdd (NextEven n) m = NextOdd (evenPlusOdd n m)

-- | Proves odd + even = odd
oddPlusEven :: Odd n -> Even m -> Odd (Add n m)
oddPlusEven OneOdd ZeroEven = OneOdd
oddPlusEven OneOdd m = evenPlusOne m
oddPlusEven (NextOdd n) m = NextOdd (oddPlusEven n m)

-- | Multiplies two natural numbers.
type family Mult (n :: Nat) (m :: Nat) :: Nat

type instance Mult Z m = Z

type instance Mult (S n) m = Add (Mult n m) m

evenTimesTwo :: Even n -> Even (Add n n)
evenTimesTwo n = evenPlusEven n n

-- | Proves even * even = even
evenTimesEven :: Even n -> Even m -> Even (Mult n m)
evenTimesEven ZeroEven _ = ZeroEven
evenTimesEven (NextEven n) m =
  evenPlusEven (evenPlusEven (evenTimesEven n m) m) m

-- | Proves odd * odd = odd
oddTimesOdd :: Odd n -> Odd m -> Odd (Mult n m)
oddTimesOdd OneOdd m = m
oddTimesOdd (NextOdd n) m = evenPlusOdd (oddPlusOdd (oddTimesOdd n m) m) m

-- | Proves even * odd = even
evenTimesOdd :: Even n -> Odd m -> Even (Mult n m)
evenTimesOdd ZeroEven _ = ZeroEven
evenTimesOdd (NextEven n) m = oddPlusOdd (evenPlusOdd (evenTimesOdd n m) m) m

-- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (Mult n m)
oddTimesEven OneOdd m = m
oddTimesEven (NextOdd n) m = evenPlusEven (evenPlusEven (oddTimesEven n m) m) m
