module Effect.Aff.Jax.Class
  ( class MonadJax
  , affjax
  , retry
  , class Responds
  , responseFormat
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
  ) where

import Prelude
import Effect.Aff            (Aff)
{-
import Control.Monad.Except.Trans   (ExceptT)
import Control.Monad.List.Trans     (ListT)
import Control.Monad.Maybe.Trans    (MaybeT)
import Control.Monad.RWS.Trans      (RWST)
import Control.Monad.State.Trans    (StateT)
import Control.Monad.Writer.Trans   (WriterT)
-}
import Control.Monad.Reader.Trans   (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class    (lift)
import Data.Argonaut.Core           (Json)
import Data.Either                  (Either(..))
import Data.HTTP.Method             (Method(..))
import Data.Maybe                   (Maybe(..))
import Affjax                       as X
import Affjax                       (Request, Response, ResponseFormatError, RetryPolicy, URL, defaultRequest)
import Affjax.RequestBody           (RequestBody)
import Affjax.ResponseFormat        (ResponseFormat, json, string, ignore)

class Monad m <= MonadJax m where
  retry
    :: forall a
     . RetryPolicy
    -> (Request a -> m (Response (Either ResponseFormatError a)))
    -> (Request a -> m (Response (Either ResponseFormatError a)))
  affjax
    :: forall a
     . Request a
    -> m (Response (Either ResponseFormatError a))

instance affJax
  :: MonadJax Aff where
  affjax r    = X.request r
  retry p f   = X.retry p f

instance readerJax :: MonadJax m => MonadJax (ReaderT r m) where
  affjax r  = lift (affjax r)
  retry p f = \r -> do
    env <- ask
    lift (retry p (flip runReaderT env <<< f) r)

-- instance contJax :: MonadJax m => MonadJax (ContT r m) where
-- instance exceptJax :: MonadJax m => MonadJax (ExceptT e m) where
-- instance listJax :: MonadJax m => MonadJax (ListT m) where
-- instance maybeJax :: MonadJax m => MonadJax (MaybeT m) where
-- instance rwsJax :: MonadJax m => MonadJax (RWST r w s m) where
-- instance stateJax :: MonadJax m => MonadJax (StateT s m) where
-- instance writerJax :: MonadJax m => MonadJax (WriterT w m) where


class Responds a where
  responseFormat :: ResponseFormat a

-- instance respondsArrayBuffer:: Responds ArrayBuffer where
--   responseFormat = arrayBuffer
--
-- instance respondsBlob :: Responds Blob where
--   responseFormat = blob
--
-- instance respondsDocument :: Responds Document where
--   responseFormat = document

instance respondsJson :: Responds Json where
  responseFormat = json

instance respondsString :: Responds String where
  responseFormat = string

instance respondsIgnore :: Responds Unit where
  responseFormat = ignore


get
  :: forall m b
   . MonadJax m
  => Responds b
  => URL
  -> m (Response (Either ResponseFormatError b))
get u = affjax $ defaultRequest { url = u, responseFormat = responseFormat }

post
  :: forall m b
   . MonadJax m
  => Responds b
  => URL
  -> RequestBody
  -> m (Response (Either ResponseFormatError b))
post u c = affjax $ defaultRequest { method = Left POST, url = u, content = Just c, responseFormat = responseFormat }

post'
  :: forall m b
   . MonadJax m
  => Responds b
  => URL
  -> Maybe RequestBody
  -> m (Response (Either ResponseFormatError b))
post' u c = affjax $ defaultRequest { method = Left POST, url = u, content = c, responseFormat = responseFormat }

post_
  :: forall m
   . MonadJax m
  => URL
  -> RequestBody
  -> m (Response (Either ResponseFormatError Unit))
post_ = post

post_'
  :: forall m
   . MonadJax m
  => URL
  -> Maybe RequestBody
  -> m (Response (Either ResponseFormatError Unit))
post_' = post'

put
  :: forall m b
   . MonadJax m
  => Responds b
  => URL
  -> RequestBody
  -> m (Response (Either ResponseFormatError b))
put u c = affjax $ defaultRequest { method = Left PUT, url = u, content = Just c, responseFormat = responseFormat }

put'
  :: forall m b
   . MonadJax m
  => Responds b
  => URL
  -> Maybe RequestBody
  -> m (Response (Either ResponseFormatError b))
put' u c = affjax $ defaultRequest { method = Left PUT, url = u, content = c, responseFormat = responseFormat }

put_
  :: forall m
   . MonadJax m
  => URL
  -> RequestBody
  -> m (Response (Either ResponseFormatError Unit))
put_ = put

put_'
  :: forall m
   . MonadJax m
  => URL
  -> Maybe RequestBody
  -> m (Response (Either ResponseFormatError Unit))
put_' = put'

delete
  :: forall m b
   . MonadJax m
  => Responds b
  => URL
  -> m (Response (Either ResponseFormatError b))
delete u = affjax $ defaultRequest { method = Left DELETE, url = u, responseFormat = responseFormat }

delete_
  :: forall m
   . MonadJax m
  => URL
  -> m (Response (Either ResponseFormatError Unit))
delete_ = delete

patch
  :: forall m b
   . MonadJax m
  => Responds b
  => URL
  -> RequestBody
  -> m (Response (Either ResponseFormatError b))
patch u c = affjax $ defaultRequest { method = Left PATCH, url = u, content = Just c, responseFormat = responseFormat }

patch'
  :: forall m b
   . MonadJax m
  => Responds b
  => URL
  -> Maybe RequestBody
  -> m (Response (Either ResponseFormatError b))
patch' u c = affjax $ defaultRequest { method = Left PATCH, url = u, content = c, responseFormat = responseFormat }

patch_
  :: forall m
   . MonadJax m
  => URL
  -> RequestBody
  -> m (Response (Either ResponseFormatError Unit))
patch_ = patch

patch_'
  :: forall m
   . MonadJax m
  => URL
  -> Maybe RequestBody
  -> m (Response (Either ResponseFormatError Unit))
patch_' = patch'
