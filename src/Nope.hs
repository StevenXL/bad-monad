{-# LANGUAGE InstanceSigs #-}

module Nope where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
    NopeDotJpg
    deriving (Eq, Show, Ord)

instance Functor Nope where
    fmap :: (a -> b) -> Nope a -> Nope b
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure :: a -> Nope a
    pure _ = NopeDotJpg
    (<*>) :: Nope (a -> b) -> Nope a -> Nope b
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return :: a -> Nope a
    return = pure
    (>>=) :: Nope a -> (a -> Nope b) -> Nope b
    (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

main :: IO ()
main = do
    let trigger :: Nope (Int, String, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
