module Pinboard.API
  ( postsUpdate
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
import Control.Monad.Aff    (Aff)
import Data.Argonaut.Core   (Json, fromString)
import Data.Either          (Either(..))
import Data.Maybe           (Maybe(..))
import Data.Tuple           (Tuple(..))
import Data.DateTime        (DateTime)
import Data.Traversable     (traverse, sequence)
import Data.StrMap          as StrMap
import Network.HTTP.Affjax  (AJAX, get)
import Network.HTTP.StatusCode (StatusCode(..))

import Pinboard.API.Types (AddOptions, AllOptions, Error(..), GetOptions, New(..), Note, Old(..), Post, RecentOptions, Suggestions, Tag, Title, Url, addOptions, allOptions, getOptions, recentOptions)
import Pinboard.API.Encode (class ToQuery, AddOptions'(..), AllOptions'(..), GetOptions'(..), RecentOptions'(..), toQuery, printQuery)
import Pinboard.API.Decode (Name, decodeArray, decodeDate, decodeNumber, decodeObject, decodePosts, decodePropWith, decodeString)


-- | Returns the most recent time any bookmark was added, updated or deleted.
postsUpdate :: forall eff. Aff (ajax :: AJAX | eff) (Either Error DateTime)
postsUpdate = decode <$> get (makeUrl_ "posts/update")
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
      decodePropWith decodeDate "update_time" o


-- | Add a bookmark.
postsAdd :: forall eff. Url -> Title -> AddOptions -> Aff (ajax :: AJAX | eff) (Either Error Unit)
postsAdd url description options = decode <$> get (makeUrl "posts/add" query)
  where
    query    = toQuery [Tuple "url" url, Tuple "description" description] <> toQuery (AddOptions' options)
    decode r = validateStatus r.status *> validateCode "result_code" r.response


-- | Delete a bookmark.
postsDelete :: forall eff. Url -> Aff (ajax :: AJAX | eff) (Either Error Unit)
postsDelete url = decode <$> get (makeUrl "posts/delete" (Tuple "url" url))
  where decode r = validateStatus r.status *> validateCode "result_code" r.response


-- | Returns one or more posts on a single day matching the arguments. If
-- | no date or url is given, date of most recent bookmark will be used.
postsGet :: forall eff. GetOptions -> Aff (ajax :: AJAX | eff) (Either Error (Array Post))
postsGet options = decode <$> get (makeUrl "posts/get" (GetOptions' options))
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
      decodePropWith decodePosts "posts" o


-- | Returns a list of the user's most recent posts, filtered by tag.
postsRecent :: forall eff. RecentOptions -> Aff (ajax :: AJAX | eff) (Either Error (Array Post))
postsRecent options = decode <$> get (makeUrl "posts/recent" (RecentOptions' options))
  where decode r = validateStatus r.status *> decodePosts root r.response


-- | Returns a list of dates with the number of posts at each date.
postsDates :: forall eff. Maybe (Array Tag) -> Aff (ajax :: AJAX | eff) (Either Error (Array (Tuple DateTime Number)))
postsDates options = decode <$> get (makeUrl "posts/dates" (Tuple "tag" <$> sequence options))
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
      traverse op (StrMap.toArrayWithKey Tuple o)
    op (Tuple k v) = Tuple <$> decodeDate root (fromString k) <*> decodeNumber root v


-- | Returns all bookmarks in the user's account.
postsAll :: forall eff. AllOptions -> Aff (ajax :: AJAX | eff) (Either Error (Array Post))
postsAll options = decode <$> get (makeUrl "posts/all" (AllOptions' options))
  where decode r = validateStatus r.status *> decodePosts root r.response


-- | Returns a list of popular tags and recommended tags for a given URL. Popular tags are
-- | tags used site-wide for the url; recommended tags are drawn from the user's own tags.
postsSuggest :: forall eff. Url -> Aff (ajax :: AJAX | eff) (Either Error Suggestions)
postsSuggest url = decode <$> get (makeUrl "posts/suggest" (Tuple "url" url))
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
      { popular: _, recommended: _ } <$>
        decodePropWith (\name x -> traverse (decodeString name) =<< decodeArray name x) "popular" o <*>
        decodePropWith (\name x -> traverse (decodeString name) =<< decodeArray name x) "recommended" o


-- | Returns a full list of the user's tags along with the number of times they were used.
tagsGet :: forall eff. Aff (ajax :: AJAX | eff) (Either Error (Array (Tuple Tag Number)))
tagsGet = decode <$> get (makeUrl_ "tags/get")
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
      traverse op (StrMap.toArrayWithKey Tuple o)
    op (Tuple k v) = Tuple k <$> decodeNumber root v


-- | Delete an existing tag.
tagsDelete :: forall eff. Tag -> Aff (ajax :: AJAX | eff) (Either Error Unit)
tagsDelete tag = decode <$> get (makeUrl "tags/delete" (Tuple "tag" tag))
  where decode r = validateStatus r.status *> validateCode "result" r.response


-- | Rename an tag, or fold it in to an existing tag.
tagsRename :: forall eff. Old Tag -> New Tag -> Aff (ajax :: AJAX | eff) (Either Error Unit)
tagsRename (Old old) (New new) = decode <$> get (makeUrl "tags/rename" query)
  where
    query    = [Tuple "old" old, Tuple "new" new]
    decode r = validateStatus r.status *> validateCode "result" r.response


-- | Returns the user's secret RSS key (for viewing private feeds).
userSecret :: forall eff. Aff (ajax :: AJAX | eff) (Either Error String)
userSecret = decode <$> get (makeUrl_ "user/secret")
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
      decodePropWith decodeString "result" o


-- | Returns the user's API token (for making API calls without a password)
userApiToken :: forall eff. Aff (ajax :: AJAX | eff) (Either Error String)
userApiToken = decode <$> get (makeUrl_ "user/api_token")
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
      decodePropWith decodeString "result" o


-- | Returns a list of the user's notes
notesList :: forall eff. Aff (ajax :: AJAX | eff) (Either Error (Array Note))
notesList = decode <$> get (makeUrl_ "notes/list")
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
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


-- | Returns an individual user note. The hash property is a 20 character long sha1 hash of the note text.
notesGet :: forall eff. String -> Aff (ajax :: AJAX | eff) (Either Error Note)
notesGet id = decode <$> get (makeUrl_ ("notes/" <> id))
  where
    decode r = do
      _ <- validateStatus r.status
      o <- decodeObject root r.response
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

makeUrl_ :: String -> String
makeUrl_ path =
  "https://kputnam:4-UTi8IFpczfbI'@api.pinboard.in/v1/" <> path

makeUrl :: forall a. ToQuery a => String -> a -> String
makeUrl path q =
  "https://kputnam:4-UTi8IFpczfbI'@api.pinboard.in/v1/"
  <> path <> printQuery (toQuery (Tuple "format" "json") <> toQuery q)

validateStatus :: StatusCode -> Either Error Unit
validateStatus (StatusCode 200) = Right unit
validateStatus (StatusCode x)   = Left (ServerError x)

validateCode :: String -> Json -> Either Error Unit
validateCode name x = do
  o <- decodeObject root x
  z <- decodePropWith decodeString name o
  if z == "done"
    then Right unit
    else Left (DecodeError (name <> ": unknown status (" <> z <> ")"))

root :: Name
root = Just "<root>"
