{-# LANGUAGE ExistentialQuantification, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Control.Dangerous
    ( Exit(..)
    , Warning(..)
    , Dangerous(..)
    , Dangerously(..)
    , warn
    , throw
    , throw_
    , stop
    , stop_
    , succeeded
    , exited
    , stopped
    , failed
    , warnings
    ) where

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader

-- | Errors
data Exit = forall e. (Show e) => Failure e
          | forall s. (Show s) => Stop s

data Warning = forall w. (Show w) => Warning w

instance Show Exit where
    show (Stop s) = "Stop: " ++ show s
    show (Failure e) = "Error: " ++ show e

instance Show Warning where
    show (Warning w) = "Warning: " ++ show w

class (Monad m) => Errorable m where
    warn :: (Show w) => w -> m ()

    exit :: Exit -> m a
    exit_ :: Exit -> m ()
    exit_ x = exit x >> return ()

    throw :: (Show e) => e -> m a
    throw e = exit (Failure e)
    throw_ :: (Show e) => e -> m ()
    throw_ e = throw e >> return ()

    stop :: (Show s) => s -> m a
    stop e = exit (Stop e)
    stop_ :: (Show s) => s -> m ()
    stop_ s = stop s >> return ()

-- | Dangerous!
data Dangerous a = Dangerous { runDangerous :: (Either Exit a, [Warning]) }

instance Functor Dangerous where
    fmap f (Dangerous (Right v, ws)) = Dangerous (Right (f v), ws)
    fmap _ (Dangerous (Left e, ws)) = Dangerous (Left e, ws)

instance Monad Dangerous where
    fail s = Dangerous (Left $ Failure s, [])
    return x = Dangerous (Right x, [])
    (Dangerous (Left e, ws)) >>= _ = Dangerous (Left e, ws)
    (Dangerous (Right v, ws)) >>= f = Dangerous $ case f v of
        Dangerous (Right v', ws') -> (Right v', ws ++ ws')
        Dangerous (Left e, ws') -> (Left e, ws ++ ws')

instance Errorable Dangerous where
    warn w = Dangerous (Right (), [Warning w])
    exit x = Dangerous (Left x, [])


-- | An adverb.
data Dangerously m a = Dangerously {
    runDangerously :: m (Either Exit a, [Warning]) }


instance (Functor m) => Functor (Dangerously m) where
    fmap f (Dangerously mapable) = Dangerously (fmap over mapable) where
        over (Left e, ws) = (Left e, ws)
        over (Right v, ws) = (Right $ f v, ws)


instance (Monad m) => Monad (Dangerously m) where
    fail s = Dangerously (return (Left $ Failure s, []))
    return = lift . return
    x >>= f = joinT (fmapM f x) where


-- The Dangerously monad helper functions.
-- These make a lot more sense to me than does '>>='
joinT :: (Monad m) => Dangerously m (Dangerously m a) -> Dangerously m a
joinT (Dangerously wrapped) = Dangerously $ wrapped >>= \r -> case r of
    (Left e, ws) -> return (Left e, ws)
    (Right (Dangerously wrapped'), ws) -> wrapped' >>= \r' -> case r' of
        (Left e', ws') -> return (Left e', ws ++ ws')
        (Right v, ws') -> return (Right v, ws ++ ws')
-- fmap without fmap!
-- Allows us to fmap Dangerously on monads that aren't functors
fmapM :: (Monad m) => (a -> b) -> Dangerously m a -> Dangerously m b
fmapM f (Dangerously wrapped) = Dangerously $ wrapped >>= \(r, ws) ->
    return $ case r of
        Left e -> (Left e, ws)
        Right v -> (Right (f v), ws)


instance MonadTrans Dangerously where
    lift x = Dangerously $ x >>= (\v -> return (Right v, []))


instance (Monad m) => Errorable (Dangerously m) where
    warn w = Dangerously $ return (Right (), [Warning w])
    exit x = Dangerously $ return (Left x, [])


instance (MonadIO m) => MonadIO (Dangerously m) where
  liftIO = lift . liftIO


succeeded :: (Either Exit a, [Warning]) -> Bool
succeeded (Right _, _) = True
succeeded _ = False

exited :: (Either Exit a, [Warning]) -> Bool
exited (Left _, _) = True
exited _ = False

stopped :: (Either Exit a, [Warning]) -> Bool
stopped (Left (Stop _), _) = True
stopped _ = False

failed :: (Either Exit a, [Warning]) -> Bool
failed (Left (Failure _), _) = True
failed _ = False

warnings :: (Either Exit a, [Warning]) -> [Warning]
warnings = snd
