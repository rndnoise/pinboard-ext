module Pinboard.API
  ( AuthToken
  , authToken
  , notesGet
  , notesList
  , postsAdd
  , postsAll
  , postsDates
  , postsDelete
  , postsGet
  , postsRecent
  , postsSuggest
  , postsUpdate
  , tagsDelete
  , tagsGet
  , tagsRename
  , userApiToken
  , userSecret
  , module Pinboard.API.Types
  ) where

import Prelude
import Chrome.Storage.Storable    (class ToStorable)
import Control.Monad.Jax.Class    (class MonadJax, affjax)
import Data.Argonaut.Core         (Json, fromString)
import Data.Argonaut.Parser       (jsonParser)
import Data.DateTime              (DateTime)
import Data.Either                (Either(..), either)
import Data.Maybe                 (Maybe(..))
import Data.StrMap                (toArrayWithKey)
import Data.Traversable           (traverse, sequence)
import Data.Tuple                 (Tuple(..), fst)
import Global                     (readFloat)
import Network.HTTP.Affjax        (AffjaxRequest, defaultRequest)
import Network.HTTP.StatusCode    (StatusCode(..))
import Unsafe.Coerce              (unsafeCoerce)

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

-------------------------------------------------------------------------------

newtype AuthToken = AuthToken String

authToken :: String -> AuthToken
authToken = AuthToken

instance showAuthToken :: Show AuthToken where
  show (AuthToken x) = x

instance toStorableAuthToken :: ToStorable AuthToken where
  toStorable = unsafeCoerce

-------------------------------------------------------------------------------

-- | Returns the most recent time any bookmark was added, updated or deleted.
postsUpdate
  :: forall m
   . MonadJax m
  => AuthToken
  -> m (Either Error DateTime)
postsUpdate auth =
  map decode (makeReq_ auth "posts/update" # affjax)
  where
    decode r = do
      validateStatus r.status
      decodePropWith decodeDate "update_time"
        =<< decodeObject root
        =<< decodeJson r.response


-- | Add a bookmark.
postsAdd
  :: forall m
   . MonadJax m
  => AuthToken
  -> Url
  -> Title
  -> AddOptions
  -> m (Either Error Unit)
postsAdd auth url description options =
  map decode (makeReq auth "posts/add" query # affjax)
  where
    decode r = do
      validateStatus r.status
      validateCode "result_code" =<< decodeJson r.response

    query =
      toQuery
      [ Tuple "url" url
      , Tuple "description" description
      ] <> toQuery (AddOptions' options)


-- | Delete a bookmark.
postsDelete
  :: forall m
   . MonadJax m
  => AuthToken
  -> Url
  -> m (Either Error Unit)
postsDelete auth url =
  map decode (makeReq auth "posts/delete" (Tuple "url" url) # affjax)
  where
    decode r = do
      validateStatus r.status
      validateCode "result_code"
        =<< decodeJson r.response


-- | Returns one or more posts on a single day matching the arguments. If
-- | no date or url is given, date of most recent bookmark will be used.
postsGet
  :: forall m
   . MonadJax m
  => AuthToken
  -> GetOptions
  -> m (Either Error (Array Post))
postsGet auth options =
  map decode (makeReq auth "posts/get" (GetOptions' options) # affjax)
  where
    decode r = do
      validateStatus r.status
      decodePropWith decodePosts "posts"
        =<< decodeObject root
        =<< decodeJson r.response


-- | Returns a list of the user's most recent posts, filtered by tag.
postsRecent
  :: forall m
   . MonadJax m
  => AuthToken
  -> RecentOptions
  -> m (Either Error (Array Post))
postsRecent auth options =
  map decode (makeReq auth "posts/recent" (RecentOptions' options) # affjax)
  where
    decode r = do
      validateStatus r.status
      decodePosts root
        =<< decodeJson r.response


-- | Returns a list of dates with the number of posts at each date.
postsDates
  :: forall m
   . MonadJax m
  => AuthToken
  -> Maybe (Array Tag)
  -> m (Either Error (Array (Tuple DateTime Number)))
postsDates auth options =
  map decode (makeReq auth "posts/dates" query # affjax)
  where
    query = Tuple "tag" <$> sequence options
    decode r = do
      validateStatus r.status
      traverse op
        <<< toArrayWithKey Tuple
        =<< decodeObject root
        =<< decodeJson r.response

    op (Tuple k v) =
      Tuple <$> decodeDate root (fromString k)
            <*> decodeNumber root v


-- | Returns all bookmarks in the user's account.
postsAll
  :: forall m
   . MonadJax m
  => AuthToken
  -> AllOptions
  -> m (Either Error (Array Post))
postsAll auth options =
  map decode (makeReq auth "posts/all" (AllOptions' options) # affjax)
  where
    decode r = do
      validateStatus r.status
      decodePosts root =<< decodeJson r.response


-- | Returns a list of popular tags and recommended tags for a given URL.
-- | Popular tags are tags used site-wide for the url; recommended tags
-- | are drawn from the user's own tags.
postsSuggest
  :: forall m
   . MonadJax m
  => AuthToken
  -> Url
  -> m (Either Error Suggestions)
postsSuggest auth url =
  map decode (makeReq auth "posts/suggest" (Tuple "url" url) # affjax)
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root =<< decodeJson r.response
      { popular: _, recommended: _ }
        <$> flip decodePropWith "popular" (\name x ->
              traverse (decodeString name) =<< decodeArray name x) o
        <*> flip decodePropWith "recommended" (\name x ->
              traverse (decodeString name) =<< decodeArray name x) o


-- | Returns a full list of the user's tags along with the number of
-- | times they were used.
tagsGetWithCounts
  :: forall m
   . MonadJax m
  => AuthToken
  -> m (Either Error (Array (Tuple Tag Number)))
tagsGetWithCounts auth =
  map decode (makeReq_ auth "tags/get" # affjax)
  where
    decode r = do
      validateStatus r.status
      traverse op
        <<< toArrayWithKey Tuple
        =<< decodeObject root =<< decodeJson r.response

    op (Tuple k v) =
      Tuple k <$> map readFloat (decodeString root v)


-- | Returns a full list of the user's tags
tagsGet
  :: forall m
   . MonadJax m
  => AuthToken
  -> m (Either Error (Array Tag))
tagsGet auth =
  map (map fst) <$> tagsGetWithCounts auth


-- | Delete an existing tag.
tagsDelete
  :: forall m
   . MonadJax m
  => AuthToken
  -> Tag
  -> m (Either Error Unit)
tagsDelete auth tag =
  map decode (makeReq auth "tags/delete" (Tuple "tag" tag) # affjax)
  where
    decode r = do
      validateStatus r.status
      validateCode "result" =<< decodeJson r.response


-- | Rename an tag, or fold it in to an existing tag.
tagsRename
  :: forall m
   . MonadJax m
  => AuthToken
  -> Old Tag
  -> New Tag
  -> m (Either Error Unit)
tagsRename auth (Old old) (New new) =
  map decode (makeReq auth "tags/rename" query # affjax)
  where
    query = [Tuple "old" old, Tuple "new" new]
    decode r = do
      validateStatus r.status
      validateCode "result" =<< decodeJson r.response


-- | Returns the user's secret RSS key (for viewing private feeds).
userSecret
  :: forall m
   . MonadJax m
  => AuthToken
  -> m (Either Error String)
userSecret auth =
  map decode (makeReq_ auth "user/secret" # affjax)
  where
    decode r = do
      validateStatus r.status
      decodePropWith decodeString "result"
        =<< decodeObject root
        =<< decodeJson r.response


-- | Returns the user's API token (for making API calls without a password)
userApiToken
  :: forall m
   . MonadJax m
  => AuthToken
  -> m (Either Error AuthToken)
userApiToken auth =
  map decode (makeReq_ auth "user/api_token" # affjax)
  where
    decode r = do
      validateStatus r.status
      map authToken
        $ decodePropWith decodeString "result"
        =<< decodeObject root
        =<< decodeJson r.response


-- | Returns a list of the user's notes
notesList
  :: forall m
   . MonadJax m
  => AuthToken
  -> m (Either Error (Array Note))
notesList auth =
  map decode (makeReq_ auth "notes/list" # affjax)
  where
    decode r = do
      validateStatus r.status
      traverse decodeNote
        =<< decodePropWith decodeArray "notes"
        =<< decodeObject root
        =<< decodeJson r.response

    decodeNote x = do
      o <- decodeObject (Just "notes") x
      { id: _, hash: _, title: _, length: _, createdAt: _, updatedAt: _, text: "" }
        <$> decodePropWith decodeString "id" o
        <*> decodePropWith decodeString "hash" o
        <*> decodePropWith decodeString "title" o
        <*> decodePropWith decodeNumber "length" o -- TODO
        <*> decodePropWith decodeDate "created_at" o
        <*> decodePropWith decodeDate "updated_at" o


-- | Returns an individual user note. The hash property is a 20 character
-- | long sha1 hash of the note text.
notesGet
  :: forall m
   . MonadJax m
  => AuthToken
  -> String
  -> m (Either Error Note)
notesGet auth id =
  map decode (makeReq_ auth ("notes/" <> id) # affjax)
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root =<< decodeJson r.response
      { id: _, text: _, hash: _, title: _, length: _, createdAt: _, updatedAt: _ }
        <$> decodePropWith decodeString "id" o
        <*> decodePropWith decodeString "text" o
        <*> decodePropWith decodeString "hash" o
        <*> decodePropWith decodeString "title" o
        <*> decodePropWith decodeNumber "length" o
        <*> decodePropWith decodeDate "created_at" o
        <*> decodePropWith decodeDate "updated_at" o

-------------------------------------------------------------------------------

baseUrl :: String
baseUrl = "https://api.pinboard.in/v1/"


-- | Construct a request that has no query parameters
makeReq_
  :: AuthToken
  -> String
  -> AffjaxRequest Unit
makeReq_ (AuthToken auth) path =
  defaultRequest
  { url = baseUrl <> path <> printQuery query

  -- Pinboard will send WWW-Authenticate response header if the auth_token
  -- is blank, unless we send HTTP BasicAuth params. That header will make
  -- Firefox display the ugly modal password prompt.
  , username = if auth == "" then Just "null" else Nothing
  , password = if auth == "" then Just "null" else Nothing
  }
  where
    query =
      toQuery
      [ Tuple "format" "json"
      , Tuple "auth_token" auth ]


-- | Construct a request with given query parameters
makeReq
  :: forall q
   . ToQuery q
  => AuthToken
  -> String
  -> q
  -> AffjaxRequest Unit
makeReq (AuthToken auth) path params =
  defaultRequest
  { url = baseUrl <> path <> printQuery query
  , username = if auth == "" then Just "null" else Nothing
  , password = if auth == "" then Just "null" else Nothing
  }
  where
    query =
      toQuery
      [ Tuple "format" "json"
      , Tuple "auth_token" auth ]
      <> toQuery params


validateStatus :: StatusCode -> Either Error Unit
validateStatus s = case s of
  StatusCode 200  -> Right unit
  StatusCode code -> Left (HttpError code)


validateCode :: String -> Json -> Either Error Unit
validateCode name x = do
  o <- decodeObject root x
  c <- decodePropWith decodeString name o
  if c == "done"
    then Right unit
    else Left (UserError c)


decodeJson :: String -> Either Error Json
decodeJson = either (Left <<< JsonError) Right <<< jsonParser


root :: Name
root = Just "root"
