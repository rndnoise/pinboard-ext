module Pinboard.Background
  ( main
  ) where

import Prelude
import Effect                       (Effect)
import Effect.Aff                   (Aff, launchAff_)
import Effect.Class                 (liftEffect)
import WebExtensions.Omnibox        as CO
import WebExtensions.Omnibox.Events as CO
import WebExtensions.Tabs           as CT
import Data.Maybe                   (Maybe(..), fromMaybe)
import Data.String.Pattern          (Pattern(..))
import Data.String.CodeUnits        (indexOf)
import Pinboard.API                 (userName)
import Pinboard.Config              (Config, loadConfig)
import Effect.Console               (log)

main :: Effect Unit
main = do
  CO.onInputStarted $ do
    CO.setDefaultSuggestion (CO.text "Search pinboard")

  CO.onInputCancelled $ do
    pure unit

  CO.onInputChanged $ \input suggest -> do
    CO.setDefaultSuggestion (CO.text "Search pinboard: " <> CO.text input)

  CO.onInputEntered $ \input disposition -> launchAff_ $ do
    conf <- loadConfig :: Aff (Config Maybe)
    let uid = fromMaybe "" (userName conf.authToken)
        url = if input `startsWith` Pattern "https://pinboard.in"
                then input
                else "https://pinboard.in/search/" <> uid <> "?query=" <> input
    case disposition of
      "currentTab"       -> CT.update_ Nothing (CT.updateOptions { url = Just url })
      "newForegroundTab" -> CT.create_ (CT.createOptions { url = Just url, active = Just true })
      "newBackgroundTab" -> CT.create_ (CT.createOptions { url = Just url, active = Just false })
      _                  -> pure unit

startsWith :: String -> Pattern -> Boolean
startsWith str prefix =
  case indexOf prefix str of
    Just 0 -> true
    _      -> false
