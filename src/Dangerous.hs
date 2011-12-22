{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
module Dangerous where
import Control.Monad.Error
import Control.Monad.State

data Warning = forall w. Show w => Warning w
instance Show Warning where show (Warning w) = "WARNING: " ++ show w

data Failure = forall e. (Error e, Show e) => Failure e
instance Show Failure where show (Failure e) = "ERROR: " ++ show e
instance Error Failure where strMsg s = Failure s

type Dangerous = ErrorT Failure (State [Warning])
type Dangerously = ErrorT Failure (StateT [Warning])

runDangerous :: Dangerous a -> (Either Failure a, [Warning])
runDangerous d = runState (runErrorT d) []

runDangerously :: Dangerously m a -> m (Either Failure a, [Warning])
runDangerously d = runStateT (runErrorT d) []


-- warn :: (MonadState [Warning] m, MonadTrans t) => Warning -> t m ()
-- warn w = (lift . modify) (++ [w])

testerr :: Dangerous Int
testerr = do
    -- warn "one"
    -- _ <- throwError $ Failure "what"
    -- warn "two"
    -- warn "three"
    Dangerous (return 0)
