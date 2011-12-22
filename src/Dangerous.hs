module Dangerous where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.Fix()
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor

data Dangerous a = D (Either a String) [String]

working :: Dangerous a -> Bool
working (D (Left _) _) = False
working (D (Right _) _) = True

withWarnings :: [String] -> Dangerous a -> Dangerous a
withWarnings ws (D r ws') = D r (ws ++ ws')

instance Functor Dangerous where
    fmap fn (D (Left _) _) = Working (fn a) ws
    fmap _ (Failed e ws) = Failed e ws

instance Monad Dangerous where
    return a = Working a []

    (Working a ws) >>= fn = withWarnings ws (fn a)
    (Failed e ws) >>= _ = withWarnings ws (Failed e ws)

    fail e = Failed e []


newtype Dangerously m a = Dangerously { runDangerously :: m (Dangerous a) }

instance (Functor m) => Functor (Dangerously m) where
    fmap fn = Dangerously . fmap (fmap fn) . runDangerously

instance (Functor m, Monad m) => Monad (Dangerously m) where
    return = lift . return
    x >>= f = Dangerously $ do
        value <- runDangerously x
        case value of
            Failed e ws -> return $ Failed e ws
            Working a ws -> withWarnings ws <$> runDangerously (f a)

instance MonadTrans Dangerously where
    lift x = Dangerously (liftM return x)

{-
instance (MonadIO m) => MonadIO (Dangerously m) where
    liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (Dangerously m) where
    ask = lift ask
    local f m = Dangerously (local f (Dangerously m))

instance MonadState s m => MonadState s (Dangerously m) where
  get = lift get
  put = lift . put
  -}
