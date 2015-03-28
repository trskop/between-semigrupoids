{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Generalization of "between" pattern.
-- Copyright:    (c) 2015 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- This module introduces generalized version of @(f .) . (. h)@ pattern.
module Data.Function.Between.Semigroupoid
    (
    -- * Between combinator
      between
    , (~@~)
    , (~@@~)

    -- * Derived combinators
    , (<~@~>)
    , (<~@@~>)
    , (~@~>)
    , (<~@~)
    )
  where

import Data.Function (flip)

import Data.Semigroupoid (Semigroupoid(o))
import Data.Semifunctor (Semifunctor(semimap))


-- | @'between' f g = (f `o`) `o` (`o` g)@
between :: Semigroupoid s => s c d -> s a b -> s b c -> s a d
between f g = (f `o`) `o` (`o` g)

-- | Infix version of 'between'.
(~@~) :: Semigroupoid s => s c d -> s a b -> s b c -> s a d
(~@~) = between

-- | Flipped variant of '~@~'.
(~@@~) :: Semigroupoid s => s a b -> s c d -> s b c -> s a d
(~@@~) = flip between

(<~@~>)
    :: (Semifunctor f c s, Semifunctor f' c' s)
    => c a b -> c' a' b'
    -> s (f' b') (f a) -> s (f' a') (f b)
f <~@~> g = semimap f `between` semimap g

-- | Flipped variant of '<~@~>'.
(<~@@~>)
    :: (Semifunctor f c s, Semifunctor f' c' s)
    => c' a' b' -> c a b
    -> s (f' b') (f a) -> s (f' a') (f b)
(<~@@~>) = flip (<~@~>)

(~@~>)
    :: Semifunctor f c s
    => s a b -> c a' b'
    -> s (f b') a -> s (f a') b
f ~@~> g = f `between` semimap g

(<~@~)
    :: Semifunctor f c s
    => c a b -> s a' b'
    -> s b' (f a) -> s a' (f b)
f <~@~ g = semimap f `between` g
