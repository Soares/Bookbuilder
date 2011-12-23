{-# LANGUAGE ExistentialQuantification, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Dangerous where
import Control.Monad.Trans
import Control.Monad.Error hiding (fail)
import Control.Monad.State hiding (fail)
import Data.Functor.Identity

-- | Errors
data Failure = forall e. (Error e, Show e) => Failure e
instance Show Failure where show (Failure e) = "ERROR: " ++ show e
data Warning = forall w. (Show w) => Warning w
instance Show Warning where show (Warning w) = "WARNING: " ++ show w
class (Monad m) => Errorable m where
	warn :: (Show w) => w -> m ()
	throw :: (Error e, Show e) => e -> m ()

-- | Dangerous!
data Dangerous a = Dangerous { runDangerous :: (Either Failure a, [Warning]) }

instance Functor Dangerous where
	fmap f (Dangerous (Right v, ws)) = Dangerous (Right (f v), ws)
	fmap _ (Dangerous (Left e, ws)) = Dangerous (Left e, ws)

join :: Dangerous (Dangerous a) -> Dangerous a
join (Dangerous (Right (Dangerous (r, ws')), ws)) = Dangerous (r, ws ++ ws')
join (Dangerous (Left e, ws)) = Dangerous (Left e, ws)

instance Monad Dangerous where
	-- TODO: lol fail
	return x = Dangerous (Right x, [])
	(Dangerous (Left e, ws)) >>= _ = Dangerous (Left e, ws)
	(Dangerous (Right v, ws)) >>= f = Dangerous $ case f v of
		Dangerous (Right v', ws') -> (Right v', ws ++ ws')
		Dangerous (Left e, ws') -> (Left e, ws ++ ws')

instance Errorable Dangerous where
	warn w = Dangerous (Right (), [Warning w])
	throw e = Dangerous (Left $ Failure e, [])

-- | An adverb.
data Dangerously m a = Dangerously {
	runDangerously :: m (Either Failure a, [Warning]) }

-- TODO: wtf? make the reqs be Functor m.
instance (Functor m) => Functor (Dangerously m) where
	fmap f (Dangerously mapable) = Dangerously (fmap over mapable) where
		over (Left e, ws) = (Left e, ws)
		over (Right v, ws) = (Right $ f v, ws)

instance (Functor m, Monad m) => Monad (Dangerously m) where
	-- todo: FAIL
	return = lift . return
	x >>= f = joinT (fmap f x)

instance MonadTrans Dangerously where
	lift x = Dangerously $ x >>= (\v -> return (Right v, []))

joinT :: (Monad m) => Dangerously m (Dangerously m a) -> Dangerously m a
joinT (Dangerously getter) = Dangerously $ getter >>= \r -> case r of
	(Left e, ws) -> return (Left e, ws)
	(Right (Dangerously getter'), ws) -> getter' >>= \r' -> case r' of
		(Left e', ws') -> return (Left e', ws ++ ws')
		(Right v, ws') -> return (Right v, ws ++ ws')


{-
testerr :: Dangerous Int
testerr = do
	warn "one"
	warn 2
	throw "err"
	warn "three"
	return 2
	-}

{-
instance Monad Dangerous where
	return x = Dangerous (Right x, [])
	(Dangerous (Right v, ws)) >>= f = case f v of
		Dangerous (Right v', ws') -> Dangerous (Right v', ws ++ ws')
		Dangerous (Left e, ws) -> Dangerous (Left e, ws)
		-- in Dangerous $ runState (runErrorT runner) ws
	(Dangerous (Left e, ws)) >>= _ = Dangerous (Left e, ws)





data Dangerous a = forall e w. (Error e, Show e, Show w) =>
	Dangerous (ErrorT e (State [w]) a)

runDangerous :: (forall e w. (Error e, Show e, Show w) =>
	(Either e a, [w]) -> r) -> Dangerous a -> r
runDangerous f (Dangerous x) = f $ runState (runErrorT x) []

instance Monad Dangerous where
	return = Dangerous . return
	x >>= f = runDangerous (>>= f) x

{-
data Dangerously m a = forall e w. (Error e, Show e, Show w) =>
	Dangerously (ErrorT e (StateT [w] m) a)
-}

{-
runDangerously :: (Monad m) => (forall e w. (Error e, Show e, Show w) =>
	(Either e a, [w]) -> m r) -> Dangerously m a -> m r
runDangerously f (Dangerously x) = (runStateT (runErrorT x) []) >>= f
-}

-- warn :: (MonadState [a] m, MonadTrans t) => a -> t m ()
warn w = (lift . modify) (++ [w])

data MyError = Foo String | Bar Int deriving Show
instance Error MyError where
    noMsg = Bar 0

testerr :: ErrorT MyError (State [String]) Int
testerr = do
    warn "one"
    _ <- throwError (Foo "hello")
    _ <- throwError (Bar 10)
    warn "two"
    warn "three"
    return 0
	-}
