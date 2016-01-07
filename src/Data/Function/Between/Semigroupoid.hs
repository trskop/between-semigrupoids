{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Generalization of "between" pattern.
-- Copyright:    (c) 2015-2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- This module introduces generalized version of @(f .) . (. h)@ pattern in
-- terms of 'Semigroupoid'.
module Data.Function.Between.Semigroupoid
    (
    -- * Between combinator

    -- | Captures common pattern of @(\\g -> f '.' g '.' h)@, but in terms of
    -- 'Semigroupoid' @(\\g -> f `o` g `o` h) where @f@ and @h@ are fixed
    -- parameters.
      between
    , (~@~)
    , (~@@~)

    -- ** Lifted Combinators
    --
    -- | Combinators based on '~@~' and '~@@~' that use 'semimap' to lift one
    -- or more of its arguments to operate in 'Semifunctor' context.
    , (<~@~>)
    , (<~@@~>)
    , (~@~>)
    , (<~@~)
    )
  where

import Data.Function (flip)

import Data.Semigroupoid (Semigroupoid(o))
import Data.Semifunctor (Semifunctor(semimap))


-- | Core combinator of this module and we build others on top of. It also has
-- an infix form '~@~' and flipped infix form '~@@~'.
--
-- This function Defined as:
--
-- @
-- 'between' f g = (f `o`) `o` (`o` g)
-- @
between :: Semigroupoid s => s c d -> s a b -> s b c -> s a d
between f g = (f `o`) `o` (`o` g)

-- | Infix version of 'between'.
--
-- @
-- f '~@~' g = (f `o`) `o` (`o` g)
-- @
(~@~) :: Semigroupoid s => s c d -> s a b -> s b c -> s a d
(~@~) = between

-- | Flipped variant of '~@~', i.e. flipped infix variant of 'between'.
(~@@~) :: Semigroupoid s => s a b -> s c d -> s b c -> s a d
(~@@~) = flip between

-- | Convenience wrapper for:
--
-- @
-- \\f g -> 'semimap' f '~@~' 'semimap' g
-- @
--
-- Name of '<~@~>' simply says that we apply 'semimap' to both its arguments
-- and then we apply '~@~'.
(<~@~>)
    :: (Semifunctor f c s, Semifunctor f' c' s)
    => c a b -> c' a' b'
    -> s (f' b') (f a) -> s (f' a') (f b)
f <~@~> g = semimap f `between` semimap g

-- | Flipped variant of '<~@~>'.
--
-- Name of '<~@@~>' simply says that we apply 'semimap' to both its arguments
-- and then we apply '~@@~'.
(<~@@~>)
    :: (Semifunctor f c s, Semifunctor f' c' s)
    => c' a' b' -> c a b
    -> s (f' b') (f a) -> s (f' a') (f b)
g <~@@~> f = semimap f `between` semimap g

-- | Apply 'semimap' to first argument of '~@~'. Dual to '~@~>' which applies
-- 'semimap' to second argument.
--
-- Defined as:
--
-- @
-- f '<~@~' g = 'semimap' f '~@~' g
-- @
--
-- Name of '<~@~' simply says that we apply 'semimap' to first (left) argument
-- and then we apply '~@~'.
(~@~>)
    :: Semifunctor f c s
    => s a b -> c a' b'
    -> s (f b') a -> s (f a') b
f ~@~> g = f `between` semimap g

-- | Apply 'semimap' to second argument of '~@~'. Dual to '<~@~' which applies
-- 'semimap' to first argument.
--
-- Defined as:
--
-- @
-- f '~@~>' g -> f '~@~' 'semimap' g
-- @
--
-- Name of '~@~>' simply says that we apply 'semimap' to second (right)
-- argument and then we apply '~@~'.
(<~@~)
    :: Semifunctor f c s
    => c a b -> s a' b'
    -> s b' (f a) -> s a' (f b)
f <~@~ g = semimap f `between` g
