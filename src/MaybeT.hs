module MaybeT where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just

  -- The signature of (>>=), specialized to MaybeT m:
  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing    -> return Nothing
      Just value -> runMaybeT $ f value

instance Monad m => Alternative (MaybeT m) where
  empty   = MaybeT $ return Nothing
  x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                        case maybe_value of
                              Nothing    -> runMaybeT y
                              Just _     -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just