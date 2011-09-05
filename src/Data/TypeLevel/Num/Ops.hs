{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeOperators,
             FlexibleInstances, FlexibleContexts, UndecidableInstances,
             EmptyDataDecls, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel.Num.Ops
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTC, non-standard instances)
--
-- Type-level numerical operations and its value-level reflection functions.
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Num.Ops 
(
  -- * Successor/Predecessor
  Succ, succ,
  Pred, pred,
  -- * Addition/Subtraction
  Add, (+),
  Sub, (-),
  -- * Multiplication/Division
  Mul, (*),
  Div, div,
  Mod, mod,
  --DivMod, divMod,
  IsDivBy, isDivBy,
  -- ** Special efficiency cases
  Mul10, mul10,
  Div10, div10,
  DivMod10, divMod10,
  -- * Exponientiation/Logarithm
  ExpBase, (^),
  -- Not implemented
  -- LogBase, logBase,
  -- LogBaseF, logBaseF,
  -- IsPowOf, isPowOf,
  -- ** Special efficiency cases
  Exp10, exp10,
  Log10, log10,
  -- * Comparison assertions
  -- ** General comparison assertion
  Trich, trich,
  -- *** Type-level values denoting comparison results
  LT, EQ, GT,
  OrderingEq, NatEq,
  -- ** Abbreviated comparison assertions
  (:>:), (:<:), (:>=:), (:<=:),
  (>)  , (<)  , (>=)  , (<=), 
  -- * Maximum/Minimum
  Max, max,
  Min, min,
  -- * Greatest Common Divisor
  GCD, gcd
) 
where

import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Sets
import Data.TypeLevel.Bool

import Prelude hiding 
 (succ, pred, (+), (-), (*), div, mod, divMod, (^), logBase,
  (==), (>), (<), (<), (>=), (<=), max, min, gcd, Bool)

-------------------------
-- Successor, Predecessor
-------------------------

-- | Successor type-level relation. @Succ x y@ establishes
--  that @succ x = y@.
-- Assoc notes: Cannot avoid malformed types
type family Succ n
type instance Succ D0 = D1
type instance Succ D1 = D2
type instance Succ D2 = D3
type instance Succ D3 = D4
type instance Succ D4 = D5
type instance Succ D5 = D6
type instance Succ D6 = D7
type instance Succ D7 = D8
type instance Succ D8 = D9
type instance Succ D9 = (D1 :* D0)
type instance Succ (x:*D0) = (x:*D1)
type instance Succ (x:*D1) = (x:*D2)
type instance Succ (x:*D2) = (x:*D3)
type instance Succ (x:*D3) = (x:*D4)
type instance Succ (x:*D4) = (x:*D5)
type instance Succ (x:*D5) = (x:*D6)
type instance Succ (x:*D6) = (x:*D7)
type instance Succ (x:*D7) = (x:*D8)
type instance Succ (x:*D8) = (x:*D9)
type instance Succ (x:*D9) = (Succ x:*D0)




-- | value-level reflection function for the 'Succ' type-level relation
succ :: x -> Succ x
succ = undefined

type family Pred n
type instance Pred D1 = D0
type instance Pred D2 = D1
type instance Pred D3 = D2
type instance Pred D4 = D3
type instance Pred D5 = D4
type instance Pred D6 = D5
type instance Pred D7 = D6
type instance Pred D8 = D7
type instance Pred D9 = D8
type instance Pred (D1:*D0) = D9
type instance Pred (D2:*D0) = (D1:*D9)
type instance Pred (D3:*D0) = (D2:*D9)
type instance Pred (D4:*D0) = (D3:*D9)
type instance Pred (D5:*D0) = (D4:*D9)
type instance Pred (D6:*D0) = (D5:*D9)
type instance Pred (D7:*D0) = (D6:*D9)
type instance Pred (D8:*D0) = (D7:*D9)
type instance Pred (D9:*D0) = (D8:*D9)
type instance Pred (xd:*xm:*D0) = Pred(xd:*xm):*D9
type instance Pred (xd:*D1) = (xd:*D0)
type instance Pred (xd:*D2) = (xd:*D1)
type instance Pred (xd:*D3) = (xd:*D2)
type instance Pred (xd:*D4) = (xd:*D3)
type instance Pred (xd:*D5) = (xd:*D4)
type instance Pred (xd:*D6) = (xd:*D5)
type instance Pred (xd:*D7) = (xd:*D6)
type instance Pred (xd:*D8) = (xd:*D7)
type instance Pred (xd:*D9) = (xd:*D8)

pred :: x -> Pred x
pred = undefined

--
----------------------
---- Add and Subtract
----------------------
--
type family Div10 m 
type instance Div10 D0 = D0
type instance Div10 D1 = D0
type instance Div10 D2 = D0
type instance Div10 D3 = D0
type instance Div10 D4 = D0
type instance Div10 D5 = D0
type instance Div10 D6 = D0
type instance Div10 D7 = D0
type instance Div10 D8 = D0
type instance Div10 D9 = D0
type instance Div10 (x:*D0) = x
type instance Div10 (x:*D1) = x
type instance Div10 (x:*D2) = x
type instance Div10 (x:*D3) = x
type instance Div10 (x:*D4) = x
type instance Div10 (x:*D5) = x
type instance Div10 (x:*D6) = x
type instance Div10 (x:*D7) = x
type instance Div10 (x:*D8) = x
type instance Div10 (x:*D9) = x
div10 :: x -> Div10 x
div10 = undefined

type family Mod10 n
type instance Mod10 D0 = D0
type instance Mod10 D1 = D1
type instance Mod10 D2 = D2
type instance Mod10 D3 = D3
type instance Mod10 D4 = D4
type instance Mod10 D5 = D5
type instance Mod10 D6 = D6
type instance Mod10 D7 = D7
type instance Mod10 D8 = D8
type instance Mod10 D9 = D9
type instance Mod10 (xd:*xm) = xm
mod10 :: x -> Mod10 x
mod10 = undefined

type family DivMod10 n
type instance DivMod10 n = (Div10 n, Mod10 n)

type family Add m n :: *
type instance Add D0 x = x
type instance Add D1 x = (Succ x)
type instance Add D2 x = Add D1 (Succ x)
type instance Add D3 x = Add D2 (Succ x)
type instance Add D4 x = Add D3 (Succ x)
type instance Add D5 x = Add D4 (Succ x)
type instance Add D6 x = Add D5 (Succ x)
type instance Add D7 x = Add D6 (Succ x)
type instance Add D8 x = Add D7 (Succ x)
type instance Add D9 x = Add D8 (Succ x)
type instance Add (xd :* xm) y = Add xm ((Add xd (Div10 y)) :* (Mod10 y))


-- | value-level reflection function for the 'Add' type-level relation 
(+) :: x -> y -> Add x y
(+) = undefined


-- --| Subtraction type-level relation. @Sub x y z@ establishes
-- -- that @x - y = z@ 
type family Sub x y
type instance Sub x D0 = x
type instance Sub x D1 = (Pred x) 
type instance Sub x D2 = Sub (Pred x) D1
type instance Sub x D3 = Sub (Pred x) D2
type instance Sub x D4 = Sub (Pred x) D3
type instance Sub x D5 = Sub (Pred x) D4
type instance Sub x D6 = Sub (Pred x) D5
type instance Sub x D7 = Sub (Pred x) D6
type instance Sub x D8 = Sub (Pred x) D7
type instance Sub x D9 = Sub (Pred x) D8
type instance Sub x (xd :* xm) = Sub (Pred x) (Pred (xd:*xm))

-- | value-level reflection function for the 'Sub' type-level relation 
(-) :: x -> y -> Sub x y
(-) = undefined
--
--------------------------------
---- Multiplication and Division
--------------------------------
--
-------------------
---- Multiplication
-------------------
--
---- | Multiplication type-level relation. @Mul x y z@ establishes
----  that @x * y = z@.
type family Mul m n
type instance Mul D0 y = D0
type instance Mul D1 y = y
type instance Mul D2 y = Add y y
type instance Mul D3 y = Add y (Mul D2 y)
type instance Mul D4 y = Add y (Mul D3 y)
type instance Mul D5 y = Add y (Mul D4 y)
type instance Mul D6 y = Add y (Mul D5 y)
type instance Mul D7 y = Add y (Mul D6 y)
type instance Mul D8 y = Add y (Mul D7 y)
type instance Mul D9 y = Add y (Mul D8 y)
-- Note that this is only valid if xd is positive.
type instance Mul (xd :* xm) y = Add (Mul xm y) ((Mul xd y) :* D0)

-- | value-level reflection function for the multiplication type-level relation 
(*) :: x -> y -> Mul x y
(*) = undefined


--
--
-------------
---- Division
-------------

-- | Division and Remainder type-level relation. @DivMod x y q r@ establishes
--  that @x/y = q + r/y@

-- division + modulo
-- x/y | y > x = (0,x)
-- x/y | y <= x = ( 1 + (x-y / y), mod (x - y))

-- This doesn't work
--type instance Div x y = Cond (y :>: x) D0 (Succ (Div (Sub x y) y))
--type instance Mod x y = Cond (y :>: x) x  (Mod (Sub x y) y)

type family Div' x y x_gt_y
type instance Div' x y False = D0
type instance Div' x y True = Succ (Div' (Sub x y) y ((Sub x y) :>=: y)) 
type family Div x y
type instance Div x y = Div' x y (Trich x y)

type family Mod' x y x_gt_y
type instance Mod' x y False = x
type instance Mod' x y True = Mod' (Sub x y) y ((Sub x y) :>=: y)
type family Mod x y
type instance Mod x y = Mod' x y (x :>=: y)


-- | value-level reflection function for the 'DivMod' type-level relation
divMod :: x -> y -> (Div x y, Mod x y)
divMod _ _ = (undefined)

-- | value-level reflection function for the 'Div' type-level relation 
div :: x -> y -> Div x y
div = undefined

-- | value-level reflection function for the 'Mod' type-level relation 
mod :: x -> y -> Mod x y
mod = undefined


------------------------------------------
---- Multiplication/Division special cases
------------------------------------------

-- | Multiplication by 10 type-level relation (based on 'DivMod10').
--   @Mul10 x y@ establishes that @10 * x = y@.
type family Mul10 n
type instance Mul10 x = (x :* D0)

-- | value-level reflection function for 'Mul10' 
mul10 :: x -> Mul10 x
mul10 = undefined


---- | value-level reflection function for DivMod10 
divMod10 :: x -> (Div10 x, Mod10 x)
divMod10 _ = (undefined, undefined)

--
------------------------------
---- Is-Divisible-By assertion
------------------------------

-- | Is-divisible-by type-level assertion. e.g @IsDivBy d x@ establishes that
--   @x@ is divisible by @d@.

-- here we use a class for demonstration purposes
class (Pos d, Nat x) => IsDivBy d x
instance (Pos d, Nat x, Mod x d ~ D0) => IsDivBy d x

-- | value-level reflection function for IsDivBy
isDivBy :: IsDivBy d x => d -> x -> ()
isDivBy _ _ = ()

-----------------------------
---- Exponentiation/Logarithm
-----------------------------
--
---- | Exponentation type-level relation. @ExpBase b e r@ establishes
----  that @b^e = r@
type family ExpBase b e
type instance ExpBase b D0 = D1
type instance ExpBase b D1 = b
type instance ExpBase b D2 = (Mul b b)
type instance ExpBase b D3 = (Mul b (ExpBase b D2))
type instance ExpBase b D4 = (Mul b (ExpBase b D3))
type instance ExpBase b D5 = (Mul b (ExpBase b D4))
type instance ExpBase b D6 = (Mul b (ExpBase b D5))
type instance ExpBase b D7 = (Mul b (ExpBase b D6))
type instance ExpBase b D8 = (Mul b (ExpBase b D7))
type instance ExpBase b D9 = (Mul b (ExpBase b D8))
type instance ExpBase b (ei :* el) = Mul b (ExpBase b (Pred (ei:* el)))

-- | value-level reflection function for the ExpBase type-level relation
(^) :: b -> e -> ExpBase b e
(^) = undefined

---------------- LEFT OUT FOR NOW ---------------------------------

-- Logarithm type-level relation. @LogBase b x e@ establishes that 
-- @log_base_b x = e@
--  Note it is not relational (i.e. cannot be used to express exponentiation)
--class (Pos b, b :>=: D2, Pos x, Nat e) =>  LogBase b x e  | b x -> e 
--instance  LogBaseF b x e f => LogBase b x e
--
--
---- | value-level reflection function for LogBase
--logBase :: LogBaseF b x e f => b -> x -> e
--logBase = undefined 
--
--
---- | Version of LogBase which also outputs if the logarithm
---- calculated was exact.
---- f indicates if the resulting logarithm has no fractional part (i.e.
---- tells if the result provided is exact)
--class (Pos b, b :>=: D2, Pos x, Nat e, Bool f) 
--     =>  LogBaseF b x e f | b x -> e f
--instance (Trich x b cmp, LogBaseF' b x e f cmp) => LogBaseF b x e f
--
--
--class (Pos b, b :>=: D2, Pos x, Nat e, Bool f)
--     => LogBaseF' b x e f cmp | b x cmp -> e f 
--instance (Pos b, b :>=: D2, Pos x) => LogBaseF' b x D0 False LT
--instance (Pos b, b :>=: D2) => LogBaseF' b b D1 True  EQ
--instance (Pos b, b :>=: D2, Pos x, DivMod x b q r, IsZero r rz, And rz f' f, 
--          Pred e e', LogBaseF b q e' f') => LogBaseF' b x e f GT
--
---- | value-level reflection function for LogBaseF
--logBaseF :: LogBaseF b x e f => b -> x -> (e,f)
--logBaseF _ _ = (undefined, undefined) 
--

-- We could reuse LogBaseF for IsPowOf but it would be inneficient.
-- LogBaseF continues calculating the logarithm even if after knowing its
-- not exact. Thus, it is desirable to include a custom definition of
-- IsPowOf which can "abort" the calculation forcing the Divisions to be
-- exact


-- | Assert that a number (@x@) can be expressed as the power of another one
--   (@b@) (i.e. the fractional part of @log_base_b x = 0@, or, 
--   in a different way, @exists y . b\^y = x@). 
--
--class (Pos b, b :>=: D2, Pos x) =>  IsPowOf b x
--instance (Trich x b cmp, IsPowOf' b x cmp) => IsPowOf b x
--class (Pos b, b :>=: D2, Pos x) => IsPowOf' b x cmp
---- If lower (x < b), then the logarithm is not exact  
---- instance (Pos b, b :>=: D2, Pos x) => IsPowOf' b x LT
--instance (Pos b, b :>=: D2) => IsPowOf' b b EQ
--instance (Pos b, b :>=: D2, Pos x, DivMod x b q D0, IsPowOf b q) 
--         => IsPowOf' b x  GT
---- | 
--isPowOf :: IsPowOf b x => b -> x -> ()
--isPowOf = undefined

-------------------------------------
---- Base-10 Exponentiation/Logarithm
-------------------------------------

type family Exp10 x
type instance Exp10 D0 = D1
type instance Exp10 D1 = (D1 :* D0)
type instance Exp10 D2 = (D1 :* D0 :* D0)
type instance Exp10 D3 = (D1 :* D0 :* D0 :* D0)
type instance Exp10 D4 = (D1 :* D0 :* D0 :* D0 :* D0)
type instance Exp10 D5 = (D1 :* D0 :* D0 :* D0 :* D0 :* D0)
type instance Exp10 D6 = (D1 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)
type instance Exp10 D7 = (D1 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)
type instance Exp10 D8 = (D1 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)
type instance Exp10 D9 = (D1 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0 :* D0)
type instance Exp10 (xi :* xl) = (Exp10 (Pred (xi:*xl)) :* D0)

-- | value-level reflection function for Exp10
exp10 :: x -> Exp10 x
exp10 = undefined

-- | Base-10 logarithm type-level relation
--   Note it is not relational (cannot be used to express Exponentation to 10)
--   However, it works with any positive numeral (not just powers of 10)
type family Log10 x 
type instance Log10 D1 = D0
type instance Log10 D2 = D0
type instance Log10 D3 = D0
type instance Log10 D4 = D0
type instance Log10 D5 = D0
type instance Log10 D6 = D0
type instance Log10 D7 = D0
type instance Log10 D8 = D0
type instance Log10 D9 = D0
type instance Log10 (xi :* xl) = Pred (Log10 xi)

-- | value-level reflection function for 'Log10'
log10 :: x -> Log10 x
log10 = undefined
--
--{- Log10': Alternative implementation of Log10
--
--Relational, but it only works for results of Exp10 (i.e. powers of 10).
--
--class (Pos x, Nat y) => Log10' x y | x -> y, y -> x
--instance Exp10 x y => Log10' y x
---}
--
--
---------------
---- Comparison
---------------

-- type-level values denoting comparison results
-- | Lower than 
data LT
-- | Equal
data EQ
-- | Greater than
data GT

type family OrderingEq o1 o2
type instance OrderingEq LT LT = True
type instance OrderingEq LT EQ = False
type instance OrderingEq LT GT = False
type instance OrderingEq EQ EQ = True
type instance OrderingEq EQ LT = False
type instance OrderingEq EQ GT = False
type instance OrderingEq GT GT = True
type instance OrderingEq GT LT = False
type instance OrderingEq GT EQ = False

-- | Trichotomy type-level relation. 'Trich x y r' establishes
--   the relation (@r@) between @x@ and @y@. The obtained relation (@r@)
--   Can be 'LT' (if @x@ is lower than @y@), 'EQ' (if @x@ equals @y@) or
--   'GT' (if @x@ is greater than @y@)
type family Trich x y

-- | value-level reflection function for the comparison type-level assertion 
trich :: x -> y -> Trich x y
trich = undefined

-- by structural induction on the first, and then the second argument
-- D0
type instance Trich D0 D0 = EQ
type instance Trich D0 D1 = LT
type instance Trich D0 D2 = LT
type instance Trich D0 D3 = LT
type instance Trich D0 D4 = LT
type instance Trich D0 D5 = LT
type instance Trich D0 D6 = LT
type instance Trich D0 D7 = LT
type instance Trich D0 D8 = LT
type instance Trich D0 D9 = LT
type instance Trich D0 (yi :* yl) = LT
type instance Trich (yi :* yl) D0 = GT
-- D1
type instance Trich D1 D0 = GT
type instance Trich D1 D1 = EQ
type instance Trich D1 D2 = LT
type instance Trich D1 D3 = LT 
type instance Trich D1 D4 = LT
type instance Trich D1 D5 = LT 
type instance Trich D1 D6 = LT
type instance Trich D1 D7 = LT 
type instance Trich D1 D8 = LT
type instance Trich D1 D9 = LT
type instance Trich D1 (yi :* yl) = LT
type instance Trich (yi :* yl) D1 = GT
-- D2
type instance Trich D2 D0 = GT
type instance Trich D2 D1 = GT
type instance Trich D2 D2 = EQ
type instance Trich D2 D3 = LT
type instance Trich D2 D4 = LT
type instance Trich D2 D5 = LT
type instance Trich D2 D6 = LT
type instance Trich D2 D7 = LT
type instance Trich D2 D8 = LT
type instance Trich D2 D9 = LT
type instance Trich D2 (yi :* yl) = LT
type instance Trich (yi :* yl) D2 = GT
-- D3
type instance Trich D3 D0 = GT
type instance Trich D3 D1 = GT
type instance Trich D3 D2 = GT
type instance Trich D3 D3 = EQ
type instance Trich D3 D4 = LT
type instance Trich D3 D5 = LT
type instance Trich D3 D6 = LT
type instance Trich D3 D7 = LT
type instance Trich D3 D8 = LT
type instance Trich D3 D9 = LT
type instance Trich D3 (yi :* yl) = LT
type instance Trich (yi :* yl) D3 = GT
-- D4
type instance Trich D4 D0 = GT
type instance Trich D4 D1 = GT
type instance Trich D4 D2 = GT
type instance Trich D4 D3 = GT
type instance Trich D4 D4 = EQ
type instance Trich D4 D5 = LT
type instance Trich D4 D6 = LT
type instance Trich D4 D7 = LT
type instance Trich D4 D8 = LT
type instance Trich D4 D9 = LT
type instance Trich D4 (yi :* yl) = LT
type instance Trich (yi :* yl) D4 = GT
-- D5
type instance Trich D5 D0 = GT
type instance Trich D5 D1 = GT
type instance Trich D5 D2 = GT
type instance Trich D5 D3 = GT
type instance Trich D5 D4 = GT
type instance Trich D5 D5 = EQ
type instance Trich D5 D6 = LT
type instance Trich D5 D7 = LT
type instance Trich D5 D8 = LT
type instance Trich D5 D9 = LT
type instance Trich D5 (yi :* yl) = LT
type instance Trich (yi :* yl) D5 = GT
-- D6
type instance Trich D6 D0 = GT
type instance Trich D6 D1 = GT
type instance Trich D6 D2 = GT
type instance Trich D6 D3 = GT
type instance Trich D6 D4 = GT
type instance Trich D6 D5 = GT
type instance Trich D6 D6 = EQ
type instance Trich D6 D7 = LT
type instance Trich D6 D8 = LT
type instance Trich D6 D9 = LT
type instance Trich D6 (yi :* yl) = LT
type instance Trich (yi :* yl) D6 = GT
-- D7
type instance Trich D7 D0 = GT
type instance Trich D7 D1 = GT
type instance Trich D7 D2 = GT
type instance Trich D7 D3 = GT
type instance Trich D7 D4 = GT
type instance Trich D7 D5 = GT
type instance Trich D7 D6 = GT
type instance Trich D7 D7 = EQ
type instance Trich D7 D8 = LT
type instance Trich D7 D9 = LT
type instance Trich D7 (yi :* yl) = LT
type instance Trich (yi :* yl) D7 = GT
-- D8
type instance Trich D8 D0 = GT
type instance Trich D8 D1 = GT
type instance Trich D8 D2 = GT
type instance Trich D8 D3 = GT
type instance Trich D8 D4 = GT
type instance Trich D8 D5 = GT
type instance Trich D8 D6 = GT
type instance Trich D8 D7 = GT
type instance Trich D8 D8 = EQ
type instance Trich D8 D9 = LT
type instance Trich D8 (yi :* yl) = LT
type instance Trich (yi :* yl) D8 = GT
-- D9
type instance Trich D9 D0 = GT
type instance Trich D9 D1 = GT
type instance Trich D9 D2 = GT
type instance Trich D9 D3 = GT
type instance Trich D9 D4 = GT
type instance Trich D9 D5 = GT
type instance Trich D9 D6 = GT
type instance Trich D9 D7 = GT
type instance Trich D9 D8 = GT
type instance Trich D9 D9 = EQ
type instance Trich D9 (yi :* yl) = LT
type instance Trich (yi :* yl) D9 = GT


-- multidigit comparison
type instance Trich (xd :* xm) (yd :* ym) = CS (Trich xd yd) (Trich xm ym)

-- strengthen the comparison relation
type family CS c1 c2
type instance CS EQ r = r
type instance CS GT r = GT
type instance CS LT r = LT

-- Abbreviated comparison assertions

-- | Equality abbreviated type-level assertion
type family NatEq x y
type instance NatEq x y = OrderingEq (Trich x y) EQ

-- | Greater-than abbreviated type-level assertion
type family x :>: y
type instance x :>: y = OrderingEq (Trich x y) GT

-- | value-level reflection function for >
(>) :: x -> y -> x :>: y
(>) = undefined

-- | Lower-than abbreviated type-level assertion
type family x :<: y
type instance x :<: y = OrderingEq (Trich x y) LT

-- | value-level reflection function for >
(<) :: x -> y -> x :<: y
(<) = undefined

-- | Greater-than or equal abbreviated type-level assertion
type family x :>=: y
type instance x :>=: y = (Succ x) :>: y

-- | value-level reflection function for >=
(>=) :: x -> y -> x :>=: y
(>=) = undefined

-- | Less-than or equal abbreviated type-level assertion
type family x :<=: y
type instance x :<=: y = x :<: (Succ y)

-- | value-level reflection function for >=
(<=) :: x -> y -> x :<=: y
(<=) = undefined


--------------------
---- Maximum/Minimum
--------------------
type family Max x y
type instance Max x y = Cond (x :>: y) x y
type family Min x y
type instance Min x y = Cond (x :<=: y) x y

-- | value-level reflection function for the maximum type-level relation
max :: x -> y -> Max x y
max = undefined

-- | value-level reflection function for the minimum type-level relation
min :: x -> y -> Min x y
min = undefined

---------
---- GCD
---------

-- | Greatest Common Divisor type-level relation
type family GCD x y
type instance GCD x y = GCD' x y (IsZero y) (Trich x y)

---- Euclidean algorithm 
--class (Nat x, Nat y, Nat gcd) => GCD' x y yz cmp gcd | x y yz cmp -> gcd
type family GCD' x y ys cmp
type instance GCD' x D0 True cmp = D0
type instance GCD' x y False LT = GCD y x
type instance GCD' x y False EQ = x
type instance GCD' x y False GT = GCD (Sub x y) y

-- | value-level reflection function for the GCD type-level relation
gcd :: x -> y -> GCD x y
gcd = undefined

-----------------------
---- Internal functions
-----------------------
--
-- classify a natural as positive or zero
type family IsZero n
type instance IsZero D0 = True
type instance IsZero D1 = False
type instance IsZero D2 = False
type instance IsZero D3 = False
type instance IsZero D4 = False
type instance IsZero D5 = False
type instance IsZero D6 = False
type instance IsZero D7 = False
type instance IsZero D8 = False
type instance IsZero D9 = False
-- debatable
type instance IsZero (xd:*xm) = And (IsZero xd) (IsZero xm)

-- 
-- The cond TF
type family Cond b x y
type instance Cond True  x y = x
type instance Cond False x y = y

