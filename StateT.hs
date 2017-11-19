module Control.Monad.State.StateT where

import State
import Control.Monad
import Control.Applicative

newtype StateT m s a = StateT {runStateT :: m (State s a)}

instance Monad m => Functor (StateT m s) where
    fmap f = StateT . (fmap . fmap) f . runStateT

instance Monad m => Applicative (StateT m s) where
    pure = StateT . return . return
    --(<*>) = ap

instance Monad m => Monad (StateT m s) where
    x >>= f = join $ StateT $ do
            mstate <- runStateT x
            return $ State $ \s->
                case runState mstate s of
                    (s,x') -> (s, f x')