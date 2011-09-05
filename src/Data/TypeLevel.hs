-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TypeLevel
-- Copyright   :  (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch
--                    and KTH's SAM group 
-- License     :  BSD-style (see the file src/Data/LICENSE)
-- 
-- Maintainer  :  alfonso.acosta@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module is a wrapper for all the publicly usable types and functions
-- of the type-level library.
-- 
-----------------------------------------------------------------------------
module Data.TypeLevel (module Data.TypeLevel.Num, 
                       module Data.TypeLevel.Bool) where

import Data.TypeLevel.Num
import Data.TypeLevel.Bool
