module Pinboard.Popup where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.Properties  as HP
import Halogen.VDom.Driver      as HV
import Halogen.Aff              as HA

import Debug.Trace as D
import Chrome.FFI (CHROME)
import Chrome.Tabs    as T
import Chrome.Windows as W

-- |
type State =
  { title :: String
  , url   :: String
  , desc  :: String
  , tags  :: Array String
  }

-- |
data Query k
  = Set Boolean k
  | Get (Boolean -> k)

-- |
type Input = Unit

-- |
type Message = Void

-- | This is executed when the user clicks the Pinboard toolbar icon
-- |
main :: Eff (HA.HalogenEffects (chrome :: CHROME)) Unit
main = HA.runHalogenAff do
  tabs <- T.query T.queryOptions
  --   <- D.traceAnyM tabs

  body <- HA.awaitBody
  io   <- HV.runUI ui unit body
  pure unit

ui :: forall e. H.Component HH.HTML Query Input Message (Aff e)
ui =
  H.component                         -- ComponentSpec h s f i o m
  { initialState: const initialState  -- i -> s
  , render:       render              -- s -> h Void (f Unit)
  , eval:         eval                -- f ~> (ComponentDSL s f o m)
  , receiver:     const Nothing }     -- i -> Maybe (f Unit)
  where
    initialState :: State
    initialState =
      { title: "Where have all the insects gone?"
      , url:   "http://www.sciencemag.org/news/2017/05/where-have-all-insects-gone"
      , desc:  "Fireflies, like these in a forest in the Netherlands, have disappeared from some areas in North America and Europe where they were once abundant."
      , tags:  ["type:article", "science"] }

    render :: State -> H.ComponentHTML Query
    render s =
      HH.div_
        [ HH.text "Oh."
        , HH.form []
            [ HH.label
                [ HP.for "url" ]
                [ HH.text "URL:" ]
            , HH.input
                [ HP.id_ "url"
                , HP.type_ HP.InputUrl
                , HP.value "https://begriffs.com/posts/2015-07-10-design-of-purescript-halogen.html" ]

            , HH.label
                [ HP.for "title" ]
                [ HH.text "Title:" ]
            , HH.input
                [ HP.id_ "title"
                , HP.type_ HP.InputText
                , HP.value "The Design of Purescript Halogen" ]

            , HH.label
                [ HP.for "desc" ]
                [ HH.text "Description:" ]
            , HH.input
                [ HP.id_ "desc"
                , HP.type_ HP.InputText
                , HP.value "" ]

            , HH.label
                [ HP.for "tags" ]
                [ HH.text "Tags:" ]
            , HH.select
                [ HP.id_ "tags"
                , HP.multiple true ]
                [ HH.option_ [ HH.text "type:article" ]
                , HH.option_ [ HH.text "type:blog" ]
                , HH.option_ [ HH.text "type:book" ]
                , HH.option_ [ HH.text "type:forum" ]
                , HH.option_ [ HH.text "type:photo" ]
                , HH.option_ [ HH.text "type:essay" ]
                , HH.option_ [ HH.text "type:course" ]
                , HH.option_ [ HH.text "type:interview" ]
                , HH.option_ [ HH.text "type:code" ]
                , HH.option_ [ HH.text "type:course" ]
                , HH.option_ [ HH.text "type:video" ] ]
            ] 
        ]

    eval :: Query ~> H.ComponentDSL State Query Message (Aff e)
    eval q = case q of
      Get k ->
        pure (k true)
      Set v k -> do
        pure k
