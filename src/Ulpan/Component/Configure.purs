module Ulpan.Component.Configure where

import Prelude

import Data.Array as A
import Data.Enum (enumFromTo)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype as NT
import DOM.HTML.Indexed.ButtonType (ButtonType(ButtonButton))
import DOM.HTML.Indexed.InputType (InputType(InputCheckbox))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Record (merge)
import Ulpan.Model (Configuration(..), TestOrdering, TestDirection, (^))


type State =
  { configuration ∷ Configuration
  }

data Query a
  = HandleInput Input a
  | ChangeOrdering  TestOrdering a
  | ChangeDirection TestDirection a
  | ChangeShowNotes Boolean a
  | Apply a

type Input =
  { configuration ∷ Configuration
  }

data Output
  = NotifyConfiguration Configuration

type DSL = H.ComponentDSL State Query Output Aff

component ∷ H.Component HH.HTML Query Input Output Aff
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  initialState ∷ Input → State
  initialState = merge {}

  render ∷ State → H.ComponentHTML Query
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "container-fluid") ]
      [ HH.h3
          [ HP.class_ (HH.ClassName "text-center") ]
          [ HH.text "Change Parameters" ]
      , HH.div
          [ HP.class_ (HH.ClassName "form") ]
          [ HH.div
              [ HP.class_ (HH.ClassName "form-group") ]
              [ HH.label
                  [ HP.for "param-ordering" ]
                  [ HH.text "Ordering" ]
              , HH.select
                  [ HP.id_ "param-ordering"
                  , HP.class_ (HH.ClassName "form-control")
                  , HE.onSelectedIndexChange \i → HE.input ChangeOrdering (unsafePartial fromJust $ orderings A.!! i)
                  ]
                  (orderings <#> \ordering →
                    HH.option
                      [ HP.selected $ ordering == st.configuration ^ _.testOrdering ]
                      [ HH.text (show ordering) ]
                  )
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "form-group") ]
              [ HH.label
                  [ HP.for "param-test-direction" ]
                  [ HH.text "Test Direction" ]
              , HH.select
                  [ HP.id_ "param-test-direction"
                  , HP.class_ (HH.ClassName "form-control")
                  , HE.onSelectedIndexChange \i → HE.input ChangeDirection (unsafePartial fromJust $ directions A.!! i)
                  ]
                  (directions <#> \direction →
                    HH.option
                      [ HP.selected $ direction == st.configuration ^ _.testDirection ]
                      [ HH.text (show direction) ]
                  )
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "form-check") ]
              [ HH.input
                  [ HP.id_ "show-notes"
                  , HP.class_ (HH.ClassName "form-check-input")
                  , HP.type_ InputCheckbox
                  , HP.checked $ st.configuration ^ _.showNotes
                  , HE.onChecked (HE.input ChangeShowNotes)
                  ]
              , HH.label
                  [ HP.for "show-notes"
                  , HP.class_ (HH.ClassName "form-check-label")
                  ]
                  [ HH.text "Show Notes" ]
              ]
          , HH.button
              [ HP.type_ ButtonButton
              , HP.class_ (HH.ClassName "btn btn-primary")
              , HE.onClick (HE.input_ Apply)
              ]
              [ HH.span_
                  [ HH.text "Apply"]
              ]
          ]
      ]
    where
      orderings ∷ Array TestOrdering
      orderings = enumFromTo bottom top
      directions ∷ Array TestDirection
      directions = enumFromTo bottom top



  eval ∷ Query ~> DSL
  eval (HandleInput input next) =
    H.modify_ (merge input) $> next
  eval (ChangeOrdering testOrdering next) =
    update _ { testOrdering = testOrdering } $> next
  eval (ChangeDirection testDirection next) =
    update _ { testDirection = testDirection } $> next
  eval (ChangeShowNotes showNotes next) =
    update _ { showNotes = showNotes } $> next
  eval (Apply next) = do
    configuration ← H.gets _.configuration
    H.raise (NotifyConfiguration configuration)
    pure next

  update f =
    H.modify_ \st → st { configuration = NT.over Configuration f st.configuration }
