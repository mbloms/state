module State where

import Control.Monad
import Control.Applicative

newtype State s a = State { runState :: (s -> (s,a)) }

get :: State s s
get = State $ \x-> (x,x)

put :: s -> State s ()
put s = State $ const (s,())

mkState :: s -> a -> State s a
mkState s a = State $ \_ -> (s,a)

unsafeRun :: State s a -> (s,a)
unsafeRun state = runState state undefined

instance Functor (State s) where
    fmap f state = State $ \x-> case runState state x of
        (state, val) -> (state, f val)

instance Applicative (State s) where
    pure x = State $ \s-> (s,x)
    f <*> x = State $ \s -> case runState f s of
        (s,f) -> case runState x s of
            (s,x) -> (s,f x)

instance Monad (State s) where
    x >>= f = State $ \s -> case runState x s of
        (s,x) -> runState (f x) s
        