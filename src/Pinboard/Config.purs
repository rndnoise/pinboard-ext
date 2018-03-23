module Pinboard.Config
  ( Config
  , Tag
  , loadConfig
  , saveConfig
  ) where

import Prelude
import Control.Monad.Aff              (Aff)
import Control.Monad.Except           (runExcept)
import Data.Array                     (elem, filter)
import Data.Either                    (hush)
import Data.Foreign                   (Foreign, F, readBoolean, readString)
import Data.List                      (List(..), toUnfoldable)
import Data.Maybe                     (Maybe, fromMaybe)
import Data.Sequence                  (head)
import Data.StrMap                    (StrMap, lookup, fromFoldable)
import Data.Tuple                     (Tuple(..), fst, snd)
import Data.Time.Duration             (Milliseconds(..))
import Halogen.HTML                   as HH

import Chrome.FFI                     (CHROME)
import Chrome.Storage.Local           as LS
import Chrome.Storage.Storable        (toStorable)
import Pinboard.UI.Internal.HTML      (class_)
import Pinboard.UI.Popup.Complete     as CC
import Pinboard.UI.Component.TagInput as TI

type Tag
  = Tuple String CC.Result

type Config i m =
  { tags      :: TI.Config i m
  , authToken :: String
  , defaults  :: { readLater  :: Maybe Boolean
                 , replace    :: Maybe Boolean
                 , private    :: Maybe Boolean } }


loadConfig :: forall m eff. Applicative m => Aff (chrome :: CHROME | eff) (Config Tag m)
loadConfig =
  decode <$> LS.getMulti [authToken, readLater, replace, private]
  where
    decode :: StrMap Foreign -> Config Tag m
    decode o =
      { tags:       tagConfig []
      , authToken:  fromMaybe "" (try readString =<< lookup authToken o)
      , defaults:
        { readLater: try readBoolean =<< lookup readLater o
        , replace:   try readBoolean =<< lookup replace o
        , private:   try readBoolean =<< lookup private o }}

    try :: forall a. (Foreign -> F a) -> Foreign -> Maybe a
    try f = hush <<< runExcept <<< f

saveConfig
  :: forall eff i m
   . Config i m
  -> Aff (chrome :: CHROME | eff) Unit
saveConfig cfg =
  LS.set (fromFoldable
           [ Tuple authToken (toStorable cfg.authToken)
           , Tuple readLater (toStorable cfg.defaults.readLater)
           , Tuple replace   (toStorable cfg.defaults.replace)
           , Tuple private   (toStorable cfg.defaults.private) ])


tagConfig
  :: forall m
   . Applicative m
  => Array String
  -> TI.Config Tag m
tagConfig corpus =
  { parse:        flip Tuple Nil
    -- ^ convert the text buffer to a tag

  , textValue:    fst
  -- ^ convert a tag to value that goes in an api request

  , renderChoice: HH.text <<< fst
    -- ^ render a chosen tag as html

  , renderOption: HH.span_ <<< toUnfoldable <<< map fmt <<< snd
    -- ^ render a suggested tag as html

  , suggest:      let search = CC.commonSubsequences corpus
                   in \chosen buffer ->
                     pure (map pick (filter (isnt chosen) (search buffer)))

  , showDelay:    Milliseconds 150.0
  , hideDelay:    Milliseconds 150.0 }
  where
    -- don't suggest items that have already been chosen
    isnt chosen (Tuple s _) = not (s `elem` (map fst chosen))

    -- just pick the first way an item matched, ignore rest
    pick (Tuple s rs) = Tuple s (fromMaybe Nil (head rs))

    -- highlight the letters that matched
    fmt (CC.M s) = HH.span [ class_ "matched "] [HH.text s]
    fmt (CC.U s) = HH.span [ class_ "unmatch "] [HH.text s]


authToken :: String
authToken = "pinboard.authToken"

readLater :: String
readLater = "pinboard.defaults.toRead"

replace :: String
replace = "pinboard.defaults.replace"

private :: String
private = "pinboard.defaults.private"
