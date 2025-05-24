module Type.Interop
  ( class NatInt
  , class DigitInt
  , class BoolBoolean
  ) where

import Data.Typelevel.Bool as Bool

import Data.Typelevel.Num.Reps
  ( D0, D1, D2, D3, D4
  , D5, D6, D7, D8, D9
  , type (:*)
  )

import Prim.Int
  ( class Add
  , class Mul
  )

import Prim.Boolean as Boolean

-- | A sealed bijection between `typelevel` `Nat`s and nonnegative `Int`s.

class NatInt :: Type -> Int -> Constraint
class NatInt nat int
  | nat -> int, int -> nat

instance
  ( NatInt high high'
  , DigitInt ones ones'
  , Mul high' 10 prod
  , Add prod ones' sum
  ) => NatInt (high :* ones) sum
else
instance DigitInt digit int => NatInt digit int

-- | Helper class for single digit representations.

class DigitInt :: Type -> Int -> Constraint
class DigitInt nat int
  | nat -> int, int -> nat

instance DigitInt D0 0 else
instance DigitInt D1 1 else
instance DigitInt D2 2 else
instance DigitInt D3 3 else
instance DigitInt D4 4 else
instance DigitInt D5 5 else
instance DigitInt D6 6 else
instance DigitInt D7 7 else
instance DigitInt D8 8 else
instance DigitInt D9 9

-- | Trivial conversion between `Bool` and `Boolean`.
class BoolBoolean :: Type -> Boolean -> Constraint
class Bool.Bool bool <= BoolBoolean bool boolean

instance BoolBoolean Bool.True Boolean.True else
instance BoolBoolean Bool.False Boolean.False
