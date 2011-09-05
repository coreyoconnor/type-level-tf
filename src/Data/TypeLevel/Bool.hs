{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies,
             Rank2Types, DeriveDataTypeable, FlexibleInstances,
             UndecidableInstances, FlexibleContexts,ScopedTypeVariables,
             TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel.Bool
-- Copyright   : (c) 2008 Benedikt Huber (port to Associative types (ghc 6.9+)) 
--               (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental (MPTC, non-standarad instances)
-- Portability :  non-portable
--
-- Type-level Booleans.
-- 
----------------------------------------------------------------------------
module Data.TypeLevel.Bool (
    -- * Type-level boolean values
    -- Bool, toBool,
    False, false,
    True, true,
    -- reifyBool,
    -- * Type-level boolean operations
    Not,
    And,
    Or
    -- Not, not,
    -- And, (&&),
    -- Or, (||),
    -- Xor, xor,
    -- Impl, imp,
) where

import Data.Generics (Typeable)
import Prelude hiding (Bool, not, (&&), (||), Eq)
import qualified Prelude as P

------------------------------------
-- Definition of type-level Booleans
------------------------------------
-- | True type-level value
data True deriving Typeable

instance Show True where
 show _ = "True"

-- | True value-level reflecting function
true :: True
true = undefined

-- | False type-level value
data False deriving Typeable

instance Show False where
 show _ = "False"


-- | False value-level reflecting function
false :: False
false = undefined

type family And b_0 b_1
type instance And True True   = True
type instance And True False  = False
type instance And False True  = False
type instance And False False = False

type family Or b_0 b_1
type instance Or True True   = True
type instance Or True False  = True
type instance Or False True  = True
type instance Or False False = False

type family Not b
type instance Not True  = False
type instance Not False = True

#if 0
type family Id b
type family Const a b
type instance Id a = a
type instance Const b a = b
-- | Booleans, internal version
class BoolI b  where
  toBool :: b -> P.Bool
  type Not  b
  type And  b :: * -> *
  type Or   b :: * -> *
  type Xor  b :: * -> *
  type Impl b :: * -> *
  type BoolEq b :: * -> *

-- To prevent the user from adding new instances to BoolI we do NOT export 
-- BoolI itself. Rather, we export the following proxy (Bool). 
-- The proxy entails BoolI and so can be used to add BoolI 
-- constraints in the signatures. However, all the constraints below
-- are expressed in terms of BoolI rather than the proxy. Thus, even if the 
-- user adds new instances to the proxy, it would not matter. 
-- Besides, because the following proxy instances are most general,
-- one may not add further instances without the overlapping instances 
-- extension.

-- | Type-level Booleans
class BoolI b => Bool b
  
instance BoolI b => Bool b

instance BoolI True where
 toBool _ = True
 type Not True = False
 type And True = Id
 type Or  True = Const True
 type Xor True = Not
 type Impl True = Id
 type BoolEq True = Id
 
instance BoolI False where
 toBool _ = False
 type Not False = True
 type And False = Const False
 type Or  False = Id
 type Xor False = Id
 type Impl False = Const True
 type BoolEq False = Not
-- | Reification function. In CPS style (best possible solution)
reifyBool :: P.Bool -> (forall b . Bool b => b -> r) -> r
reifyBool True  f = f true
reifyBool False f = f false

-------------
-- Operations
-------------


-- | value-level reflection function for the 'Not' type-level relation
not :: b1 -> Not b1
not = undefined

-- | 'And' type-level relation. @And b1 b2 b3@ establishes that
--   @b1 && b2 = b3@


-- | value-level reflection function for the 'And' type-level relation
(&&) :: b1 -> b2 -> And b1 b2
(&&) = undefined
infixr 3 &&
  
-- | Or type-level relation. @Or b1 b2 b3@ establishes that
--   @b1 || b2 = b3@


-- | value-level reflection function for the 'Or' type-level relation
(||) :: b1 -> b2 -> Or b1 b2
(||) = undefined
infixr 2 ||

-- | Exclusive or type-level relation. @Xor b1 b2 b3@ establishes that
--   @xor b1 b2 = b3@

-- | value-level reflection function for the 'Xor' type-level relation
xor :: b1 -> b2 -> Xor b1 b2
xor = undefined


-- | Implication type-level relation. @Imp b1 b2 b3@ establishes that
-- @b1 =>b2 = b3@

-- | value-level reflection function for the Imp type-level relation
imp :: b1 -> b2 -> Impl b1 b2
imp = undefined


-- Although equality can be defined as the composition of Xor and Not
-- we define it specifically

-- | Boolean equality type-level relation

-- FIXME: eq should be named (==) but it clashes with the (==) defined
--        in Data.TypeLevel.Num . The chosen (and ugly) workaround was 
--        to rename it to eq.

-- | value-level reflection function for the 'Eq' type-level relation
boolEq :: b1 -> b2 -> BoolEq b1 b2
boolEq = undefined
#endif

