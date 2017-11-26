{-# LANGUAGE InstanceSigs #-}

module PhbtEither where

import Prelude hiding (Left, Right)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhbtEither b a
    = Left a
    | Right b
    deriving (Eq, Show, Ord)

instance Functor (PhbtEither b) where
    fmap :: (a -> c) -> PhbtEither b a -> PhbtEither b c
    fmap f (Left a) = Left (f a)
    fmap _ (Right b) = Right b

instance Applicative (PhbtEither b) where
    pure :: a -> PhbtEither b a
    pure = Left
    (<*>) :: PhbtEither b (a -> c) -> PhbtEither b a -> PhbtEither b c
    (<*>) (Left f) (Left a) = Left (f a)
    (<*>) _ (Right b) = Right b
    (<*>) (Right b) _ = Right b

instance Monad (PhbtEither b) where
    return :: a -> PhbtEither b a
    return = pure
    (>>=) :: PhbtEither b a -> (a -> PhbtEither b c) -> PhbtEither b c
    (>>=) (Left a) f = f a
    (>>=) (Right b) _ = Right b

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhbtEither b a) where
    arbitrary :: Gen (PhbtEither b a)
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Left a, return $ Right b]

instance (Eq b, Eq a) => EqProp (PhbtEither b a) where
    (=-=) = eq

main :: IO ()
main = do
    let trigger :: PhbtEither String (Int, String, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
