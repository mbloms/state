module Data.Array.State where

import Control.Monad
import Control.Applicative
import Data.Array

newtype SArray a = SArray {runArray :: (Array Int Int -> (Array Int Int, a))}

get :: Int -> SArray Int
get i = SArray $ \arr -> (arr, arr ! i)

put :: Int -> Int -> SArray ()
put i x = SArray $ \arr -> (arr // [(i,x)], ())

runArray' :: Array Int Int -> SArray a -> (Array Int Int, a)
runArray' arr (SArray f) = f arr

instance Functor SArray where
    fmap f state = SArray $ \x-> case runArray state x of
        (state, val) -> (state, f val)
