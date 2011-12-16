module Data.Bound
    ( Bound(Bounded,Unbounded)
    , isBounded
    , isUnbounded
    , fromBounded
    , fromBound
    ) where

import Control.Monad ( liftM2 )

data Bound a = Bounded a | Unbounded
    deriving (Eq, Read, Show)

instance Ord a => Ord (Bound a) where
    x <= y = fromBound True $ liftM2 (<=) x y

instance Functor Bound where
    fmap _ Unbounded = Unbounded
    fmap f (Bounded a) = Bounded (f a)

instance Monad Bound where
    Unbounded >>= _ = Unbounded
    (Bounded x) >>= k = k x

    Unbounded >> _ = Unbounded
    (Bounded _) >> k = k

    return = Bounded
    fail _ = Unbounded

isBounded :: Bound a -> Bool
isBounded Unbounded = False
isBounded _ = True

isUnbounded :: Bound a -> Bool
isUnbounded = not . isBounded

fromBounded :: Bound a -> a
fromBounded Unbounded = error "Bound.fromBounded: Unbounded"
fromBounded (Bounded x) = x

fromBound :: a -> Bound a -> a
fromBound d Unbounded = d
fromBound _ (Bounded x) = x
