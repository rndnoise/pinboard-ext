module Pinboard.UI.Debounce
  ( Debouncer
  , newDebouncer
  , cancelDebouncer
  , resetDebouncer
  , whenQuiet
  ) where

import Prelude
import Control.Monad.Aff            (Aff, Fiber, apathize, delay, error, forkAff, killFiber)
import Control.Monad.Aff.Class      (class MonadAff, liftAff)
import Control.Monad.Aff.AVar       (AVar, putVar, takeVar, killVar, makeEmptyVar, AVAR)
import Control.Monad.Eff.Exception  (Error)
import Data.Time.Duration           (Milliseconds)


-- |
newtype Debouncer eff =
  Debouncer
  { var :: AVar Unit
  , fib :: Fiber eff Unit }

-- |
newDebouncer
  :: forall m e
   . MonadAff (avar :: AVAR | e) m
  => Milliseconds
  -> m (Debouncer (avar :: AVAR | e))
newDebouncer ms = liftAff do
  var <- makeEmptyVar
  fib <- forkAff do
    delay ms
    putVar unit var
  pure (Debouncer { var, fib })


-- |
cancelDebouncer
  :: forall m e
   . MonadAff (avar :: AVAR | e) m
  => Debouncer (avar :: AVAR | e)
  -> m Unit
cancelDebouncer (Debouncer { var, fib }) = liftAff do
  killFiber (error "canceled") fib
  killVar (error "canceled") var


-- |
resetDebouncer
  :: forall m e
   . MonadAff (avar :: AVAR | e) m
  => Debouncer (avar :: AVAR | e)
  -> Milliseconds
  -> m (Debouncer (avar :: AVAR | e))
resetDebouncer db ms = cancelDebouncer db *> newDebouncer ms


-- |
whenQuiet
  :: forall m e
   . MonadAff (avar :: AVAR | e) m
  => Debouncer (avar :: AVAR | e)
  -> m _
  -> m Unit
whenQuiet (Debouncer { var }) action =
  unit <$ (liftAff (apathize (takeVar var)) *> action)
