module Ulpan.Component.VocabSelector where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import DOM.HTML.Indexed.ButtonType (ButtonType(ButtonButton))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record (merge)
import Ulpan.Model (VocabFile)
import Ulpan.VocabLoader (loadAvailableVocabs)


type State =
  { vocabFile       ∷ Maybe VocabFile
  , availableVocabs ∷ Array VocabFile
  }

data Query a
  = Initialize a
  | HandleInput Input a
  | ChangeVocabFile (Maybe VocabFile) a
  | Apply a
  | Cancel a

type Input =
  { vocabFile ∷ Maybe VocabFile }

data Output
  = NotifyVocabFile (Maybe VocabFile)

type DSL = H.ComponentDSL State Query Output Aff

component ∷ H.Component HH.HTML Query Input Output Aff
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where

  initialState ∷ Input → State
  initialState = merge { availableVocabs : [] }

  render ∷ State → H.ComponentHTML Query
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "container-fluid") ]
      [ HH.h3
          [ HP.class_ (HH.ClassName "text-center") ]
          [ HH.text "Select Vocab File" ]
      , HH.div
          [ HP.class_ (HH.ClassName "form") ]
          [ HH.div
              [ HP.class_ (HH.ClassName "form-group") ]
              [ HH.label
                  [ HP.for "vocab-file" ]
                  [ HH.text "Select Vocab File" ]
              , HH.select
                  [ HP.id_ "vocab-file"
                  , HP.class_ (HH.ClassName "form-control")
                  , HE.onSelectedIndexChange \i → HE.input ChangeVocabFile (st.availableVocabs A.!! (i - 1))
                  ]
                $ [ HH.option
                      []
                      [ HH.text "Select..." ]
                  ]
                  <> (st.availableVocabs <#> \availableVocab →
                       HH.option
                         [ HP.selected (Just availableVocab == st.vocabFile) ]
                         [ HH.text (unwrap availableVocab).name ]
                     )
              ]
          , HH.button
              [ HP.type_ ButtonButton
              , HP.class_ (HH.ClassName "btn btn-primary")
              , HE.onClick (HE.input_ Apply)
              ]
              [ HH.span_
                  [ HH.text "Apply"]
              ]
          , HH.button
              [ HP.type_ ButtonButton
              , HP.class_ (HH.ClassName "btn btn-secondary")
              , HE.onClick (HE.input_ Cancel)
              ]
              [ HH.span_
                  [ HH.text "Cancel"]
              ]
          ]
      ]

  eval ∷ Query ~> DSL
  eval (HandleInput input next) =
    H.modify_ (merge input) $> next
  eval (Initialize next) = do
    availableVocabs ← H.liftAff loadAvailableVocabs
    H.modify_ _ { availableVocabs = availableVocabs }
    pure next
  eval (ChangeVocabFile vocabFile next) = do
    H.modify_ _ { vocabFile = vocabFile }
    pure next
  eval (Apply next) = do
    vocabFile ← H.gets _.vocabFile
    H.raise (NotifyVocabFile vocabFile)
    pure next
  eval (Cancel next) = pure next
