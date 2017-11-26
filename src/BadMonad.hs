{-# LANGUAGE InstanceSigs #-}

module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
    CountMe Integer
            a
    deriving (Eq, Show)

instance Functor CountMe where
    fmap :: (a -> b) -> CountMe a -> CountMe b
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure :: a -> CountMe a
    pure = CountMe 0
    (<*>) :: CountMe (a -> b) -> CountMe a -> CountMe b
    (<*>) (CountMe i f) (CountMe i' a) = CountMe (i + i') (f a)

instance Monad CountMe where
    return :: a -> CountMe a
    return = pure
    (>>=) :: CountMe a -> (a -> CountMe b) -> CountMe b
    (>>=) (CountMe i a) f =
        let CountMe i' b = f a
        in CountMe (i + i') b

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq

main :: IO ()
main = do
    let trigger :: CountMe (Int, String, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
