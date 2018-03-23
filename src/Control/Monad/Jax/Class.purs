module Control.Monad.Jax.Class
  ( class MonadJax
  , get
  , post
  , post'
  , post_
  , post_'
  , put
  , put'
  , put_
  , put_'
  , delete
  , delete_
  , patch
  , patch'
  , patch_
  , patch_'
  , retry
  , affjax
  ) where

import Prelude
import Data.Maybe                   (Maybe(..))
import Data.Either                  (Either(..))
import Data.HTTP.Method             (Method(..))
import Control.Monad.Aff            (Aff)
import Control.Monad.Eff.Ref        (REF)
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Except.Trans   (ExceptT)
import Control.Monad.List.Trans     (ListT)
import Control.Monad.Maybe.Trans    (MaybeT)
import Control.Monad.RWS.Trans      (RWST)
import Control.Monad.Reader.Trans   (ReaderT, class MonadReader, runReaderT, ask)
import Control.Monad.State.Trans    (StateT)
import Control.Monad.Writer.Trans   (WriterT)

import Network.HTTP.Affjax          (AJAX, AffjaxRequest, AffjaxResponse, RetryPolicy, URL, defaultRequest)
import Network.HTTP.Affjax          as X
import Network.HTTP.Affjax.Request  (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Type.Row.Effect.Equality     (class EffectRowEquals, effFrom, effTo)

class Monad m <= MonadJax m where
  retry
    :: forall a b
     . Requestable a
    => RetryPolicy
    -> (AffjaxRequest a -> m (AffjaxResponse b))
    -> (AffjaxRequest a -> m (AffjaxResponse b))
  affjax
    :: forall a b
     . Requestable a
    => Respondable b
    => AffjaxRequest a
    -> m (AffjaxResponse b)

instance affJax
  :: EffectRowEquals eff (ajax :: AJAX, ref :: REF | xyz)
  => MonadJax (Aff eff) where
  affjax r    = effFrom (X.affjax r)
  retry p f   = effFrom <<< X.retry p (effTo <<< f)

-- instance contJax :: MonadJax m => MonadJax (ContT r m) where
-- instance exceptJax :: MonadJax m => MonadJax (ExceptT e m) where
-- instance listJax :: MonadJax m => MonadJax (ListT m) where
-- instance maybeJax :: MonadJax m => MonadJax (MaybeT m) where
-- instance rwsJax :: MonadJax m => MonadJax (RWST r w s m) where

instance readerJax :: MonadJax m => MonadJax (ReaderT r m) where
  affjax r  = lift (affjax r)
  retry p f = \r -> do
    env <- ask
    lift (retry p (flip runReaderT env <<< f) r)

-- instance stateJax :: MonadJax m => MonadJax (StateT s m) where
-- instance writerJax :: MonadJax m => MonadJax (WriterT w m) where


get
  :: forall m b
   . MonadJax m
  => Respondable b
  => URL
  -> m (AffjaxResponse b)
get u = affjax $ defaultRequest { url = u }

post
  :: forall m a b
   . MonadJax m
  => Requestable a
  => Respondable b
  => URL
  -> a
  -> m (AffjaxResponse b)
post u c = affjax $ defaultRequest { method = Left POST, url = u, content = Just c }

post'
  :: forall m a b
   . MonadJax m
  => Requestable a
  => Respondable b
  => URL
  -> Maybe a
  -> m (AffjaxResponse b)
post' u c = affjax $ defaultRequest { method = Left POST, url = u, content = c }

post_
  :: forall m a
   . MonadJax m
  => Requestable a
  => URL
  -> a
  -> m (AffjaxResponse Unit)
post_ = post

post_'
  :: forall m a
   . MonadJax m
  => Requestable a
  => URL
  -> Maybe a
  -> m (AffjaxResponse Unit)
post_' = post'

put
  :: forall m a b
   . MonadJax m
  => Requestable a
  => Respondable b
  => URL
  -> a
  -> m (AffjaxResponse b)
put u c = affjax $ defaultRequest { method = Left PUT, url = u, content = Just c }

put'
  :: forall m a b
   . MonadJax m
  => Requestable a
  => Respondable b
  => URL
  -> Maybe a
  -> m (AffjaxResponse b)
put' u c = affjax $ defaultRequest { method = Left PUT, url = u, content = c }

put_
  :: forall m a
   . MonadJax m
  => Requestable a
  => URL
  -> a
  -> m (AffjaxResponse Unit)
put_ = put

put_'
  :: forall m a
   . MonadJax m
  => Requestable a
  => URL
  -> Maybe a
  -> m (AffjaxResponse Unit)
put_' = put'

delete
  :: forall m b
   . MonadJax m
  => Respondable b
  => URL
  -> m (AffjaxResponse b)
delete u = affjax $ defaultRequest { method = Left DELETE, url = u }

delete_
  :: forall m
   . MonadJax m
  => URL
  -> m (AffjaxResponse Unit)
delete_ = delete

patch
  :: forall m a b
   . MonadJax m
  => Requestable a
  => Respondable b
  => URL
  -> a
  -> m (AffjaxResponse b)
patch u c = affjax $ defaultRequest { method = Left PATCH, url = u, content = Just c }

patch'
  :: forall m a b
   . MonadJax m
  => Requestable a
  => Respondable b
  => URL
  -> Maybe a
  -> m (AffjaxResponse b)
patch' u c = affjax $ defaultRequest { method = Left PATCH, url = u, content = c }

patch_
  :: forall m a
   . MonadJax m
  => Requestable a
  => URL
  -> a
  -> m (AffjaxResponse Unit)
patch_ = patch

patch_'
  :: forall m a
   . MonadJax m
  => Requestable a
  => URL
  -> Maybe a
  -> m (AffjaxResponse Unit)
patch_' = patch'
