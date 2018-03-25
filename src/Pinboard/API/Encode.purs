module Pinboard.API.Encode
  ( AddOptions'(..)
  , AllOptions'(..)
  , GetOptions'(..)
  , RecentOptions'(..)
  , class ToQuery
  , printQuery
  , toQuery
  ) where

-------------------------------------------------------------------------------

import Prelude
import Data.Array               (catMaybes)
import Data.DateTime            (DateTime)
import Data.Foldable            (class Foldable, foldMap)
import Data.Formatter.DateTime  (Formatter, FormatterCommand(..), format)
import Data.List                as L
import Data.List                (List(..), singleton, fromFoldable)
import Data.Maybe               (Maybe(..), fromMaybe)
import Data.Profunctor.Strong   (second)
import Data.StrMap              (StrMap, fromFoldableWith, toUnfoldable)
import Data.Tuple               (Tuple(..))
import Data.URI.Common          (joinWith)
import Data.URI.Query           (Query(..))
import Global                   (encodeURIComponent)
import Pinboard.API.Types       (AddOptions, AllOptions, GetOptions, RecentOptions)

-------------------------------------------------------------------------------

class ToQuery a where
  toQuery :: a -> Query


instance encodeQuery :: ToQuery Query where
  toQuery x = x


instance encodeTuple :: ToQuery (Tuple String String) where
  toQuery (Tuple k v) = Query (singleton (Tuple k (Just v)))


instance encodeTupleM :: ToQuery(Tuple String (Maybe String)) where
  toQuery (Tuple k v) = Query (singleton (Tuple k v))


instance encodeFoldable :: Foldable f => ToQuery (f (Tuple String String)) where
  toQuery xs = Query (foldMap op xs)
    where
      op (Tuple k v) = singleton (Tuple k (Just v))


instance encodeFoldableM :: Foldable f => ToQuery (f (Tuple String (Maybe String))) where
  toQuery xs = Query (fromFoldable xs)


newtype AddOptions' = AddOptions' AddOptions
newtype GetOptions' = GetOptions' GetOptions
newtype AllOptions' = AllOptions' AllOptions
newtype RecentOptions' = RecentOptions' RecentOptions


instance encodeAddOptions :: ToQuery AddOptions' where
  toQuery (AddOptions' x) = toQuery (catMaybes
    [ Tuple "extended"                  <$> x.extended
    , Tuple "dt"       <<< fromDateTime <$> x.dt
    , Tuple "replace"  <<< fromBool     <$> x.replace
    , Tuple "shared"   <<< fromBool     <$> x.shared
    , Tuple "toread"   <<< fromBool     <$> x.toread ] <>
    map (Tuple "tags") (fromMaybe [] x.tags))


instance encodeGetOptions :: ToQuery GetOptions' where
  toQuery (GetOptions' x) = toQuery (catMaybes
    [ Tuple "dt"   <<< fromDate <$> x.dt
    , Tuple "url"               <$> x.url
    , Tuple "meta" <<< fromBool <$> x.meta ] <>
    map (Tuple "tag") (fromMaybe [] x.tag))


instance encodeRecentOptions :: ToQuery RecentOptions' where
  toQuery (RecentOptions' x) = toQuery (catMaybes
    [ Tuple "count" <<< fromInt <$> x.count ] <>
    map (Tuple "tag") (fromMaybe [] x.tag))


instance encodeAllOptions :: ToQuery AllOptions' where
  toQuery (AllOptions' x) = toQuery (catMaybes
    [ Tuple "start"    <<< fromInt       <$> x.start
    , Tuple "results"  <<< fromInt       <$> x.results
    , Tuple "fromdt"   <<< fromDateTime  <$> x.fromdt
    , Tuple "todt"     <<< fromDateTime  <$> x.todt
    , Tuple "meta"     <<< fromInt       <$> x.meta ] <>
    map (Tuple "tag") (fromMaybe [] x.tag))


fromInt :: Int -> String
fromInt x = show x


fromBool :: Boolean -> String
fromBool true  = "yes"
fromBool false = "no"


fromDate :: DateTime -> String
fromDate x = format isoDate x
  where
    -- UTC timestamp in this format: 2010-12-11
    isoDate :: Formatter
    isoDate = fromFoldable
      [ YearFull,            Placeholder "-"
      , MonthTwoDigits,      Placeholder "-"
      , DayOfMonthTwoDigits ]


fromDateTime :: DateTime -> String
fromDateTime x = format isoDateTime x
  where
    -- UTC timestamp in this format: 2010-12-11T19:48:02Z
    isoDateTime :: Formatter
    isoDateTime = fromFoldable
      [ YearFull,            Placeholder "-"
      , MonthTwoDigits,      Placeholder "-"
      , DayOfMonthTwoDigits, Placeholder "T"
      , Hours24,             Placeholder ":"
      , MinutesTwoDigits,    Placeholder ":"
      , SecondsTwoDigits,    Placeholder "Z" ]


printQuery :: Query -> String
printQuery (Query m) =
  case m of
       Nil -> ""
       xs  -> "?" <> joinWith "&" (printPart <$> toUnfoldable multiMap)
  where
    multiMap :: StrMap (List (Maybe String))
    multiMap = fromFoldableWith (<>) (map (second singleton) m)

    printPart :: Tuple String (List (Maybe String)) -> String
    printPart (Tuple k vs) =
      case L.catMaybes vs of
           Nil  -> encodeURIComponent k
           ws   -> encodeURIComponent k <> "=" <> joinWith "+" (encodeURIComponent <$> ws)
