module Pinboard.UI.Component.Debounce
  ( Debouncer
  , cancel
  , create
  , reset
  , whenQuiet
  ) where

import Prelude
import Effect.Aff            (Fiber, attempt, delay, error, forkAff, killFiber)
import Effect.Aff.AVar       (AVar, put, take, kill, empty)
import Effect.Aff.Class      (class MonadAff, liftAff)
import Data.Either           (Either(..))
import Data.Time.Duration    (Milliseconds)

-------------------------------------------------------------------------------

-- | When events are fired rapidly (eg, mouse movements or key
-- | presses), a debouncer can be used to wait for a period of
-- | inactivity before reacting to the events.
-- |
-- | One common example includes auto-complete, where a remote
-- | API call for each key press is inefficient.
newtype Debouncer =
  Debouncer
  { var :: AVar Unit
  , fib :: Fiber Unit }


-- | Construct a new debouncer
create
  :: forall m
   . MonadAff m
  => Milliseconds
  -> m Debouncer
create ms = liftAff do
  var <- empty
  fib <- forkAff do
    delay ms
    put unit var
  pure (Debouncer { var, fib })


-- | The thread waiting for this debouncer will be terminated
-- | and the debouncer will become unusable.
cancel
  :: forall m
   . MonadAff m
  => Debouncer
  -> m Unit
cancel (Debouncer { var, fib }) = liftAff do
  killFiber (error "canceled") fib
  kill (error "canceled") var


-- | The thread waiting for this debouncer will be terminated
-- | and the returned debouncer can be used with a new thread
reset
  :: forall m
   . MonadAff m
  => Debouncer
  -> Milliseconds
  -> m Debouncer
reset db ms = cancel db *> create ms


-- | Blocks until the debouncer hasn't been reset for the given
-- | amount of time. Only one thread of execution will proceed;
-- | any other threads will starve.
whenQuiet
  :: forall m a
   . MonadAff m
  => Debouncer
  -> m a
  -> m Unit
whenQuiet (Debouncer { var }) action = unit <$ do
  liftAff (attempt (take var)) >>= case _ of
    Right _ -> action $> unit
    _       -> pure unit
