module Data.Array.State where

import Control.Monad
import Control.Applicative
import Data.Array

newtype SArray a = SArray {runSArray :: (Array Int Int -> (Array Int Int, a))}

get :: Int -> SArray Int
get i = SArray $ \arr -> (arr, arr ! i)

set :: Int -> Int -> SArray ()
set i x = SArray $ \arr -> (arr // [(i,x)], ())

--run :: SArray a -> Array Int Int
run = elems. fst . runArray (array (0,10) $ map (\i-> (i,0)) [0..10])

runArray :: Array Int Int -> SArray a -> (Array Int Int, a)
runArray arr (SArray f) = f arr

instance Functor SArray where
    fmap f state = SArray $ \x-> case runSArray state x of
        (state, val) -> (state, f val)

instance Applicative SArray where
    pure x = SArray $ \s-> (s,x)
    f <*> x = SArray $ \s -> case runSArray f s of
        (s,f) -> case runSArray x s of
            (s,x) -> (s,f x)

instance Monad SArray where
    x >>= f = SArray $ \s -> case runSArray x s of
        (s,x) -> runSArray (f x) s
