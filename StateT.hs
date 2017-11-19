module Control.Monad.State.StateT where

import Control.Monad
import Control.Applicative
import Data.Array.MArray
--import Data.Maybe 
--import Data.Array.ST 
--import Control.Monad.ST 
import Data.Array.IO 
import Data.Array


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

lift :: Monad m => m a -> StateT m s a
lift c = StateT $ \s -> c >>= (\x -> return (s,x))

liftF :: Monad m => (a -> m b) -> a -> StateT m s b
liftF f x = lift (f x)

get :: Monad m => StateT m s s
get = StateT $ \x-> return (x,x)

put :: Monad m => s -> StateT m s ()
put s = StateT . const $ return (s,())

update :: (MArray a e m, Ix i) => i -> e -> StateT m (a i e) ()
update i e = StateT $ \arr -> writeArray arr i e >> return (arr,())

test = fmap snd (runStateT (lift (newArray (0,10) 0) >>= put >> update 0 5 >> get >>= liftF (freeze :: IOArray Int Int -> IO (Array Int Int))) (undefined :: IOArray Int Int))
