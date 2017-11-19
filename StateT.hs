module Control.Monad.State.StateT where

import State
import Control.Monad
import Control.Applicative

newtype StateT m s a = StateT {runStateT :: (s -> m (s,a))}

instance Monad m => Functor (StateT m s) where
    fmap f state = StateT $ \x-> do 
        (state, val) <- runStateT state x
        return (state, f val)

instance Monad m => Applicative (StateT m s) where
    pure x = StateT $ \s -> return (s,x)
    (<*>) = ap

instance Monad m => Monad (StateT m s) where
    (StateT x) >>= f = StateT $ \s -> do
        (state, val) <- x s
        runStateT (f val) state