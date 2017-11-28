{-# LANGUAGE InstanceSigs #-}

module Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a =
    Identity a
    deriving (Eq, Show, Ord)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = arbitrary >>= return . Identity

instance Monad Identity where
    return :: a -> Identity a
    return = pure
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (>>=) (Identity a) f = f a

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

main :: IO ()
main = do
    let trigger :: Identity (String, Int, String)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
