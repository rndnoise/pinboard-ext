module Pinboard.UI.Search
  where

import Prelude
import Data.Tuple               (Tuple(..), fst)
import Data.Monoid              (class Monoid, mempty, (<>))
import Data.Sequence            (Seq, empty, singleton)
import Control.Alt              (class Alt, (<|>))
import Control.Plus             (class Plus)
import Control.Alternative      (class Alternative)
import Control.MonadPlus        (class MonadPlus)
import Control.MonadZero        (class MonadZero)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)

-- |
newtype Search w a = Search (Seq (Tuple w a))

derive instance eqSearch :: (Eq w, Eq a) => Eq (Search w a)
derive instance ordSearch :: (Ord w, Ord a) => Ord (Search w a)
derive instance genericSearch :: Generic (Search w a) _
instance showSearch :: (Show w, Show a) => Show (Search w a) where show = genericShow

tell :: forall w. w -> Search w Unit
tell w = Search (singleton (Tuple w unit))

instance functorSearch
  :: Functor (Search w) where
  map f (Search xs) = Search (map (\(Tuple w a) -> Tuple w (f a)) xs)

instance altSearch
  :: Alt (Search w) where
  alt (Search xs) (Search ys) = Search (xs <|> ys)

instance plusSearch
  :: Plus (Search w) where
  empty = Search empty

instance applySearch
  :: Semigroup w
  => Apply (Search w) where
  apply (Search fs) (Search xs) = Search (op =<< fs)
    where
      op (Tuple u f) = map (\(Tuple w x) -> Tuple (u <> w) (f x)) xs

instance alternativeSearch :: Monoid w => Alternative (Search w)

instance applicativeSearch
  :: Monoid w
  => Applicative (Search w) where
  pure a = Search (singleton (Tuple mempty a))

instance bindSearch
  :: Monoid w
  => Bind (Search w) where
  bind (Search xs) f = Search (op =<< xs)
    where
      un (Search xs) = xs
      op (Tuple w a) = map (\(Tuple u a) -> Tuple (w <> u) a) (un (f a))

instance monadSearch
  :: Monoid w
  => Monad (Search w)

instance monadZeroSearch
  :: Monoid w
  => MonadZero (Search w)

instance monadPlus
  :: Monoid w
  => MonadPlus (Search w)
