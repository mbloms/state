module Data.Array.State where

import Control.Monad
import Control.Applicative
import Data.Array

newtype SArray a = SArray (Array Int Int -> (Array Int Int, a))

get :: Int -> SArray Int
get i = SArray $ \arr -> (arr, arr ! i)

put :: Int -> Int -> SArray ()
put i x = SArray $ \arr -> (arr // [(i,x)], ())

runArray :: Array Int Int -> SArray a -> (Array Int Int, a)
runArray arr (SArray f) = f arr

--instance Functor (State s) where
--    fmap f state = State $ \x-> case runState state x of
--        (state, val) -> (state, f val)
--
--instance Applicative (State s) where
--    pure x = State $ \s-> (s,x)
--    f <*> x = State $ \s -> case runState f s of
--        (s,f) -> case runState x s of
--            (s,x) -> (s,f x)
--
--instance Monad (State s) where
--    x >>= f = State $ \s -> case runState x s of
--        (s,x) -> runState (f x) s
--