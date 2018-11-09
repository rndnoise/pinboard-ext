module Pinboard.API.Types
  ( Error(..)
  , New(..)
  , Note
  , Old(..)
  , Post
  , Tag
  , Title
  , Url
  , AddOptions
  , addOptions
  , GetOptions
  , getOptions
  , RecentOptions
  , recentOptions
  , AllOptions
  , allOptions
  , Suggestions
  ) where

import Data.Maybe             (Maybe(..))
import Data.DateTime          (DateTime)
import Affjax.ResponseFormat  (ResponseFormatError)

-------------------------------------------------------------------------------

type Url = String
type Tag = String
type Title = String


newtype New a = New a
newtype Old a = Old a


data Error
  = JsonError String
  | UserError String
  | HttpError Int


type Post =
  { href        :: Url
  , description :: String
  , extended    :: String
  , hash        :: String
  , meta        :: String
  , others      :: Maybe Number
  , tags        :: Array Tag
  , time        :: DateTime
  , toread      :: Boolean
  , shared      :: Boolean }


type Note =
  { id        :: String
  , title     :: String
  , hash      :: String
  , text      :: String
  , createdAt :: DateTime
  , updatedAt :: DateTime
  , length    :: Number }


type AddOptions = 
  { extended    :: Maybe String
  , tags        :: Maybe (Array Tag)
  , dt          :: Maybe DateTime
  , replace     :: Maybe Boolean
  , shared      :: Maybe Boolean
  , toread      :: Maybe Boolean }


addOptions :: AddOptions
addOptions =
  { extended: Nothing
  , tags:     Nothing
  , dt:       Nothing
  , replace:  Nothing
  , shared:   Nothing
  , toread:   Nothing }


type GetOptions =
  { tag   :: Maybe (Array Tag)
  , dt    :: Maybe DateTime
  , url   :: Maybe String
  , meta  :: Maybe Boolean }


getOptions :: GetOptions
getOptions =
  { tag:  Nothing
  , dt:   Nothing
  , url:  Nothing
  , meta: Nothing }


type RecentOptions =
  { tag   :: Maybe (Array Tag)
  , count :: Maybe Int }


recentOptions :: RecentOptions
recentOptions =
  { tag:    Nothing
  , count:  Nothing }


type AllOptions =
  { tag     :: Maybe (Array String)
  , start   :: Maybe Int
  , results :: Maybe Int
  , fromdt  :: Maybe DateTime
  , todt    :: Maybe DateTime
  , meta    :: Maybe Int }


allOptions :: AllOptions
allOptions =
  { tag:      Nothing
  , start:    Nothing
  , results:  Nothing
  , fromdt:   Nothing
  , todt:     Nothing
  , meta:     Nothing }


type Suggestions =
  { popular     :: Array Tag
  , recommended :: Array Tag }
