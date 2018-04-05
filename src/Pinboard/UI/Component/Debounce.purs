module Pinboard.UI.Component.Debounce
  ( Debouncer
  , cancel
  , create
  , reset
  , whenQuiet
  ) where

import Prelude
import Control.Monad.Aff            (Fiber, attempt, delay, error, forkAff, killFiber)
import Control.Monad.Aff.AVar       (AVar, putVar, takeVar, killVar, makeEmptyVar, AVAR)
import Control.Monad.Aff.Class      (class MonadAff, liftAff)
import Data.Either                  (Either(..))
import Data.Time.Duration           (Milliseconds)

-------------------------------------------------------------------------------

-- | When events are fired rapidly (eg, mouse movements or key
-- | presses), a debouncer can be used to wait for a period of
-- | inactivity before reacting to the events.
-- |
-- | One common example includes auto-complete, where a remote
-- | API call for each key press is inefficient.
newtype Debouncer eff =
  Debouncer
  { var :: AVar Unit
  , fib :: Fiber (avar :: AVAR | eff) Unit }


-- | Construct a new debouncer
create
  :: forall m eff
   . MonadAff (avar :: AVAR | eff) m
  => Milliseconds
  -> m (Debouncer eff)
create ms = liftAff do
  var <- makeEmptyVar
  fib <- forkAff do
    delay ms
    putVar unit var
  pure (Debouncer { var, fib })


-- | The thread waiting for this debouncer will be terminated
-- | and the debouncer will become unusable.
cancel
  :: forall m eff
   . MonadAff (avar :: AVAR | eff) m
  => Debouncer eff
  -> m Unit
cancel (Debouncer { var, fib }) = liftAff do
  killFiber (error "canceled") fib
  killVar (error "canceled") var


-- | The thread waiting for this debouncer will be terminated
-- | and the returned debouncer can be used with a new thread
reset
  :: forall m eff
   . MonadAff (avar :: AVAR | eff) m
  => Debouncer eff
  -> Milliseconds
  -> m (Debouncer eff)
reset db ms = cancel db *> create ms


-- | Blocks until the debouncer hasn't been reset for the given
-- | amount of time. Only one thread of execution will proceed;
-- | any other threads will starve.
whenQuiet
  :: forall m eff a
   . MonadAff (avar :: AVAR | eff) m
  => Debouncer eff
  -> m a
  -> m Unit
whenQuiet (Debouncer { var }) action = unit <$ do
  liftAff (attempt (takeVar var)) >>= case _ of
    Right _ -> action $> unit
    _       -> pure unit
