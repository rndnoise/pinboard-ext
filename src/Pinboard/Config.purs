module Pinboard.Config
  ( Config
  , Defaults
  , Tag
  , loadConfig
  , saveConfig
  , saveTags
  , tagConfig
  ) where

import Prelude
import WebExtensions.Storage.Local        (getMulti, set)
import WebExtensions.Storage.Storable     (toStorable)
import Effect.Aff                         (Aff)
import Control.Monad.Except               (runExcept)
import Data.Array                         (elem, filter)
import Data.Either                        (hush)
import Data.List                          (List(..), toUnfoldable)
import Data.Maybe                         (Maybe, fromMaybe)
import Foreign                            (Foreign, F, readArray, readBoolean, readString)
import Foreign.Object                     (Object, lookup, fromFoldable)
import Data.Time.Duration                 (Milliseconds(..))
import Data.Traversable                   (traverse)
import Data.Tuple                         (Tuple(..), fst, snd)
import Halogen.HTML                       as HH
import Pinboard.API                       (AuthToken, authToken)
import Pinboard.UI.Component.TagComplete  as TC
import Pinboard.UI.Component.TagInput     as TI
import Pinboard.UI.Internal.HTML          (class_)

-------------------------------------------------------------------------------

type Tag
  = Tuple String TC.Result

type Config m =
  { tags        :: TI.Config Tag m
    -- ^ Tag auto-complete settings

  , reloadTags  :: Array String -> TI.Config Tag m
    -- ^ Creates a new auto-complete function given an array of tags

  , authToken   :: AuthToken
    -- ^ https://pinboard.in/settings/password

  , defaults    :: Defaults }

type Defaults =
  { readLater :: Boolean
    -- ^ Mark read later by default

  , replace   :: Boolean
    -- ^ Replace existing bookmarks by default

  , private   :: Boolean }
    -- ^ Make bookmarks private by default

-------------------------------------------------------------------------------

-- | Load configuration settings from localStorage
loadConfig :: forall m. Applicative m => Aff (Config m)
loadConfig =
  decode <$> getMulti [_authToken, _readLater, _replace, _private, _tags]
  where
    decode :: Object Foreign -> Config m
    decode o =
      { tags:       tagConfig (try [] readTags (lookup _tags o))
      , reloadTags: tagConfig
      , authToken:  authToken $ try "" readString (lookup _authToken o)
      , defaults:
        { readLater: try false readBoolean (lookup _readLater o)
        , replace:   try false readBoolean (lookup _replace o)
        , private:   try true  readBoolean (lookup _private o) }}

    try :: forall a. a -> (Foreign -> F a) -> Maybe Foreign -> a
    try x f m = fromMaybe x (try_ f =<< m)

    try_ :: forall a. (Foreign -> F a) -> Foreign -> Maybe a
    try_ f = hush <<< runExcept <<< f

    readTags :: Foreign -> F (Array String)
    readTags = traverse readString <=< readArray


-- | Save configuration settings to localStorage
saveConfig :: forall m. Config m -> Aff Unit
saveConfig cfg =
  set (fromFoldable
       [ Tuple _authToken (toStorable cfg.authToken)
       , Tuple _readLater (toStorable cfg.defaults.readLater)
       , Tuple _replace   (toStorable cfg.defaults.replace)
       , Tuple _private   (toStorable cfg.defaults.private) ])


-- | Save set of tags to localStorage
saveTags :: Array String -> Aff Unit
saveTags xs = set (fromFoldable [Tuple _tags (toStorable xs)])


-- | Construct auto-completion settings for given array of tags
tagConfig :: forall m. Applicative m => Array String -> TI.Config Tag m
tagConfig tags =
  { parse:        flip Tuple Nil
    -- ^ Convert the text buffer to a tag

  , textValue:    fst
    -- ^ Convert a tag to value that goes in an api request

  , renderChoice: HH.text <<< fst
    -- ^ Render a chosen tag as html

  , renderOption: HH.span_ <<< toUnfoldable <<< map fmt <<< snd
    -- ^ Render a suggested tag as html

  , suggest:      let search = TC.matches tags
                   in \chosen buffer ->
                     pure (filter (isnt chosen) (search buffer))

  , showDelay:    Milliseconds 150.0
    -- ^ Wait this long before suggesting items

  , hideDelay:    Milliseconds 150.0 }
    -- ^ Wait this long before hiding after losing focus

  where
    -- Don't suggest items that have already been chosen
    isnt chosen (Tuple s _) = not (s `elem` (map fst chosen))

    -- Highlight the letters that matched
    fmt (TC.M s) = HH.span [class_ "matched"] [HH.text s]
    fmt (TC.U s) = HH.span [class_ "unmatch"] [HH.text s]


_authToken :: String
_authToken = "pinboard.authToken"

_readLater :: String
_readLater = "pinboard.defaults.toRead"

_replace :: String
_replace = "pinboard.defaults.replace"

_private :: String
_private = "pinboard.defaults.private"

_tags :: String
_tags = "pinboard.tags"
