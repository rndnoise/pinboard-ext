module Pinboard.API
  ( AuthToken
  , postsUpdate
  , postsAdd
  , postsDelete
  , postsGet
  , postsRecent
  , postsDates
  , postsAll
  , postsSuggest
  , tagsGet
  , tagsDelete
  , tagsRename
  , userSecret
  , userApiToken
  , notesList
  , notesGet
  , module Pinboard.API.Types
  ) where

import Prelude
import Global                     (readFloat)
import Control.Monad.Jax.Class    (class MonadJax, affjax)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Argonaut.Core         (Json, fromString)
import Data.Argonaut.Parser       as J
import Data.Either                (Either(..), either)
import Data.Maybe                 (Maybe(..))
import Data.Tuple                 (Tuple(..))
import Data.DateTime              (DateTime)
import Data.Traversable           (traverse, sequence)
import Data.StrMap                (toArrayWithKey)
import Network.HTTP.Affjax        (AJAX, AffjaxRequest, defaultRequest)
import Network.HTTP.StatusCode    (StatusCode(..))

import Pinboard.API.Types
  ( AddOptions
  , addOptions
  , AllOptions
  , allOptions
  , Error(..)
  , GetOptions
  , getOptions
  , New(..)
  , Note
  , Old(..)
  , Post
  , RecentOptions
  , recentOptions
  , Suggestions
  , Tag
  , Title
  , Url )

import Pinboard.API.Encode
  ( class ToQuery
  , AddOptions'(..)
  , AllOptions'(..)
  , GetOptions'(..)
  , RecentOptions'(..)
  , toQuery
  , printQuery )

import Pinboard.API.Decode
  ( Name
  , decodeArray
  , decodeDate
  , decodeNumber
  , decodeObject
  , decodePosts
  , decodePropWith
  , decodeString )


type AuthToken = String


-- | Returns the most recent time any bookmark was added, updated or deleted.
postsUpdate
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => m (Either Error DateTime)
postsUpdate = decode <$> (affjax =<< makeReq_ "posts/update")
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      decodePropWith decodeDate "update_time" o


-- | Add a bookmark.
postsAdd
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => Url
  -> Title
  -> AddOptions
  -> m (Either Error Unit)
postsAdd url description options = decode <$> (affjax =<< makeReq "posts/add" query)
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      validateCode "result_code" j

    query = toQuery [ Tuple "url" url
                    , Tuple "description" description
                    ] <> toQuery (AddOptions' options)


-- | Delete a bookmark.
postsDelete
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => Url
  -> m (Either Error Unit)
postsDelete url = decode <$> (affjax =<< makeReq "posts/delete" (Tuple "url" url))
  where
    decode r = do
      validateStatus r.status
      j <- jsonParser r.response
      validateCode "result_code" j


-- | Returns one or more posts on a single day matching the arguments. If
-- | no date or url is given, date of most recent bookmark will be used.
postsGet
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => GetOptions
  -> m (Either Error (Array Post))
postsGet options = decode <$> (affjax =<< makeReq "posts/get" (GetOptions' options))
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      decodePropWith decodePosts "posts" o


-- | Returns a list of the user's most recent posts, filtered by tag.
postsRecent
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => RecentOptions
  -> m (Either Error (Array Post))
postsRecent options =
  decode <$> (affjax =<< makeReq "posts/recent" (RecentOptions' options))
  where
    decode r = do
      validateStatus r.status
      j <- jsonParser r.response
      decodePosts root j


-- | Returns a list of dates with the number of posts at each date.
postsDates
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m 
  => Maybe (Array Tag)
  -> m (Either Error (Array (Tuple DateTime Number)))
postsDates options =
  decode <$> (affjax =<< makeReq "posts/dates" (Tuple "tag" <$> sequence options))
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      traverse op (toArrayWithKey Tuple o)

    op (Tuple k v) =
      Tuple <$> decodeDate root (fromString k)
            <*> decodeNumber root v


-- | Returns all bookmarks in the user's account.
postsAll
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => AllOptions
  -> m (Either Error (Array Post))
postsAll options = decode <$> (affjax =<< makeReq "posts/all" (AllOptions' options))
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      decodePosts root j


-- | Returns a list of popular tags and recommended tags for a given URL.
-- | Popular tags are tags used site-wide for the url; recommended tags
-- | are drawn from the user's own tags.
postsSuggest
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => Url
  -> m (Either Error Suggestions)
postsSuggest url = decode <$> (affjax =<< makeReq "posts/suggest" (Tuple "url" url))
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      { popular: _, recommended: _ } <$>
        decodePropWith
          (\name x -> traverse (decodeString name) =<< decodeArray name x)
          "popular" o <*>
        decodePropWith
          (\name x -> traverse (decodeString name) =<< decodeArray name x)
          "recommended" o


-- | Returns a full list of the user's tags along with the number of
-- | times they were used.
tagsGet
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => m (Either Error (Array (Tuple Tag Number)))
tagsGet = decode <$> (affjax =<< makeReq_ "tags/get")
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      traverse op (toArrayWithKey Tuple o)
    op (Tuple k v) = Tuple k <$> map readFloat (decodeString root v)


-- | Delete an existing tag.
tagsDelete
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => Tag
  -> m (Either Error Unit)
tagsDelete tag = decode <$> (affjax =<< makeReq "tags/delete" (Tuple "tag" tag))
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      validateCode "result" j


-- | Rename an tag, or fold it in to an existing tag.
tagsRename
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => Old Tag
  -> New Tag
  -> m (Either Error Unit)
tagsRename (Old old) (New new) = decode <$> (affjax =<< makeReq "tags/rename" query)
  where
    query    = [Tuple "old" old, Tuple "new" new]
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      validateCode "result" j


-- | Returns the user's secret RSS key (for viewing private feeds).
userSecret
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => m (Either Error String)
userSecret = decode <$> (affjax =<< makeReq_ "user/secret")
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      decodePropWith decodeString "result" o


-- | Returns the user's API token (for making API calls without a password)
userApiToken
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => m (Either Error String)
userApiToken = decode <$> (affjax =<< makeReq_ "user/api_token")
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      decodePropWith decodeString "result" o


-- | Returns a list of the user's notes
notesList
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => m (Either Error (Array Note))
notesList = decode <$> (affjax =<< makeReq_ "notes/list")
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      n <- decodePropWith decodeArray "notes" o
      traverse decodeNote n
    decodeNote x = do
      o <- decodeObject (Just "notes") x
      { id: _, hash: _, title: _, length: _, createdAt: _, updatedAt: _, text: "" } <$>
        decodePropWith decodeString "id" o           <*>
        decodePropWith decodeString "hash" o         <*>
        decodePropWith decodeString "title" o        <*>
        decodePropWith decodeNumber "length" o       <*> -- TODO "35"
        decodePropWith decodeDate   "created_at" o   <*>
        decodePropWith decodeDate   "updated_at" o


-- | Returns an individual user note. The hash property is a 20 character
-- | long sha1 hash of the note text.
notesGet
  :: forall m
   . MonadJax m
  => MonadAsk AuthToken m
  => String
  -> m (Either Error Note)
notesGet id = decode <$> (affjax =<< makeReq_ ("notes/" <> id))
  where
    decode r = do
      _ <- validateStatus r.status
      j <- jsonParser r.response
      o <- decodeObject root j
      { id: _, text: _, hash: _, title: _, length: _, createdAt: _, updatedAt: _ } <$>
        decodePropWith decodeString "id" o          <*>
        decodePropWith decodeString "text" o        <*>
        decodePropWith decodeString "hash" o        <*>
        decodePropWith decodeString "title" o       <*>
        decodePropWith decodeNumber "length" o      <*>
        decodePropWith decodeDate   "created_at" o  <*>
        decodePropWith decodeDate   "updated_at" o

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

baseUrl :: String
baseUrl = "https://api.pinboard.in/v1/"

makeReq_
  :: forall m
   . MonadAsk AuthToken m
  => String
  -> m (AffjaxRequest Unit)
makeReq_ path = do
  authToken <- ask
  let query  = toQuery [Tuple "format" "json", Tuple "auth_token" authToken]
  pure (defaultRequest { url = baseUrl <> path <> printQuery query })

makeReq
  :: forall m a
   . MonadAsk AuthToken m
  => ToQuery a
  => String
  -> a
  -> m (AffjaxRequest Unit)
makeReq path q = do
  authToken <- ask
  let query  = toQuery [Tuple "format" "json", Tuple "auth_token" authToken] <> toQuery q
  pure (defaultRequest { url = baseUrl <> path <> printQuery query })

validateStatus :: StatusCode -> Either Error Unit
validateStatus (StatusCode 200) = Right unit
validateStatus (StatusCode x)   = Left (ServerError ("HTTP " <> show x))

validateCode :: String -> Json -> Either Error Unit
validateCode name x = do
  o <- decodeObject root x
  z <- decodePropWith decodeString name o
  if z == "done"
    then Right unit
    else Left (ServerError z)

jsonParser :: String -> Either Error Json
jsonParser = either (Left <<< DecodeError) Right <<< J.jsonParser

root :: Name
root = Just "<root>"
