module Ulpan.Component.GroupSelector where

import Prelude

import DOM.HTML.Indexed.ButtonType (ButtonType(ButtonButton))
import DOM.HTML.Indexed.InputType (InputType(InputCheckbox))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record (merge)
import Ulpan.Model (Group, Vocab, vGroups)

type State =
  { vocab  ∷ Vocab
  , groups ∷ Array Group
  }

data Query a
  = HandleInput Input a
  | ChangeGroup Group a
  | SelectAll a
  | ClearAll a
  | Apply a

type Input =
  { vocab  ∷ Vocab
  , groups ∷ Array Group
  }

data Output
  = NotifyGroups (Array Group)

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
  initialState = identity

  render ∷ State → H.ComponentHTML Query
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "container-fluid") ]
      [ HH.h3
          [ HP.class_ (HH.ClassName "text-center") ]
          [ HH.text "Select Groups" ]
      , HH.div
          [ HP.class_ (HH.ClassName "form") ]
        $ [ HH.button
              [ HP.type_ ButtonButton
              , HP.class_ (HH.ClassName "btn btn-secondary")
              , HE.onClick (HE.input_ SelectAll)
              ]
              [ HH.span_
                  [ HH.text "Select all"]
              ]
          , HH.button
              [ HP.type_ ButtonButton
              , HP.class_ (HH.ClassName "btn btn-secondary")
              , HE.onClick (HE.input_ ClearAll)
              ]
              [ HH.span_
                  [ HH.text "Clear all"]
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
        <>
          (vGroups st.vocab <#> \group ->
             HH.div
               [ HP.class_ (HH.ClassName "form-check") ]
               [ HH.input
                   [ HP.id_ (unwrap group)
                   , HP.class_ (HH.ClassName "form-check-input")
                   , HP.type_ InputCheckbox
                   , HP.checked $ A.elem group st.groups
                   , HE.onChecked (HE.input_ (ChangeGroup group))
                   ]
               , HH.label
                   [ HP.for (unwrap group)
                   , HP.class_ (HH.ClassName "form-check-label")
                   ]
                   [ HH.text (unwrap group) ]
               ]
          )
        <>
          [ HH.button
              [ HP.type_ ButtonButton
              , HP.class_ (HH.ClassName "btn btn-primary")
              , HE.onClick (HE.input_ Apply)
              ]
              [ HH.span_
                  [ HH.text "Apply"]
              ]
          ]
      ]

  eval ∷ Query ~> DSL
  eval (HandleInput input next) = do
    H.modify_ (merge input)
    pure next
  eval (ChangeGroup group next) = do
    H.modify_ \st ->
      if A.notElem group st.groups
        then st { groups = A.cons   group        st.groups }
        else st { groups = A.filter (_ /= group) st.groups }
    pure next
  eval (SelectAll next) = do
    H.modify_ \st -> st { groups = vGroups st.vocab }
    pure next
  eval (ClearAll next) = do
    H.modify_ _ { groups = [] }
    pure next
  eval (Apply next) = do
    groups ← H.gets _.groups
    H.raise (NotifyGroups groups)
    pure next
