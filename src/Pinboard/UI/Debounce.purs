module Pinboard.UI.Debounce
  ( Debouncer
  , create
  , cancel
  , reset
  , whenQuiet
  ) where

import Prelude
import Control.Monad.Aff            (Fiber, attempt, delay, error, forkAff, killFiber)
import Control.Monad.Aff.Class      (class MonadAff, liftAff)
import Control.Monad.Aff.AVar       (AVar, putVar, takeVar, killVar, makeEmptyVar, AVAR)
import Data.Either                  (Either(..))
import Data.Time.Duration           (Milliseconds)


-- |
newtype Debouncer eff =
  Debouncer
  { var :: AVar Unit
  , fib :: Fiber eff Unit }


-- |
create
  :: forall m e
   . MonadAff (avar :: AVAR | e) m
  => Milliseconds
  -> m (Debouncer (avar :: AVAR | e))
create ms = liftAff do
  var <- makeEmptyVar
  fib <- forkAff do
    delay ms
    putVar unit var
  pure (Debouncer { var, fib })


-- |
cancel
  :: forall m e
   . MonadAff (avar :: AVAR | e) m
  => Debouncer (avar :: AVAR | e)
  -> m Unit
cancel (Debouncer { var, fib }) = liftAff do
  killFiber (error "canceled") fib
  killVar (error "canceled") var


-- |
reset
  :: forall m e
   . MonadAff (avar :: AVAR | e) m
  => Debouncer (avar :: AVAR | e)
  -> Milliseconds
  -> m (Debouncer (avar :: AVAR | e))
reset db ms = cancel db *> create ms


-- |
whenQuiet
  :: forall m e
   . MonadAff (avar :: AVAR | e) m
  => Debouncer (avar :: AVAR | e)
  -> m Unit
  -> m Unit
whenQuiet (Debouncer { var }) action = unit <$ do
  x <- liftAff (attempt (takeVar var))
  case x of
       Right _ -> action
       _       -> pure unit
