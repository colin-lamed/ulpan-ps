module Ulpan.Component.App where

import Prelude
import Data.Array as A
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Enum (enumFromTo)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed.ButtonType (ButtonType(ButtonButton))
import Effect.Aff (Aff)
import Effect.Console (log, error)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Record (merge)
import Ulpan.Component.Configure as Configure
import Ulpan.Component.GroupSelector as GroupSelector
import Ulpan.Component.Test as Test
import Ulpan.Component.VocabSelector as VocabSelector
import Ulpan.LocalStorage (storeConfiguration, storeGroups, storeMode, storeVocabFile, restoreConfiguration, restoreGroups, restoreMode, restoreVocabFile)
import Ulpan.Model (Configuration(..), Group, Mode(..), TestDirection(..), TestOrdering(..), Vocab, VocabFile, vGroups)
import Ulpan.VocabLoader (loadVocab)


type State =
  { mode          ∷ Mode
  , vocabFile     ∷ Maybe VocabFile
  , vocab         ∷ Maybe Vocab
  , groups        ∷ Array Group
  , configuration ∷ Configuration
  }

data Query a
  = Initialize a
  | SetMode Mode a
  | HandleVocabSelector VocabSelector.Output a
  | HandleGroupSelector GroupSelector.Output a
  | HandleConfigure Configure.Output a

type ChildQuery =    VocabSelector.Query
                <\/> GroupSelector.Query
                <\/> Configure.Query
                <\/> Test.Query
                <\/> Const Void

type ChildSlot =  Unit
               \/ Unit
               \/ Unit
               \/ Unit
               \/ Void

type Input = Unit

type Output = Void

type DSL = H.ParentDSL State Query ChildQuery ChildSlot Output Aff

type HTML = H.ParentHTML Query ChildQuery ChildSlot Aff

component ∷ H.Component HH.HTML Query Input Output Aff
component =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where

  initialState ∷ Input → State
  initialState _ =
    -- initial state - will be updated in Initialize
    { mode          : Test
    , vocabFile     : Nothing
    , vocab         : Nothing
    , groups        : []
    , configuration : Configuration { testOrdering  : FileOrdered
                                    , testDirection : FromLanguage1
                                    , showNotes     : true
                                    }
    }

  render ∷ State → HTML
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "app app_layout") ]
      [ HH.nav
        [ HP.class_ (HH.ClassName "navbar navbar-expand-lg navbar-light bg-light") ]
        [ HH.span
            [ HP.class_ (HH.ClassName "navbar-brand") ]
            [ HH.text "Ulpan Drill" ]
        , HH.button
            [ HP.class_ (HH.ClassName "navbar-toggler")
            , HP.type_ ButtonButton
            , dataToggle "collapse"
            , dataTarget $ "#" <> collapseId
            , HPA.controls collapseId
            , HPA.expanded "false"
            , HPA.label "Toggle navigation"
            ]
            [ HH.span
                [ HP.class_ (HH.ClassName "navbar-toggler-icon") ]
                []
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "collapse navbar-collapse")
            , HP.id_ collapseId
            ]
            [ HH.ul
                [ HP.class_ (HH.ClassName "navbar-nav mr-auto") ]
                (modes <#> navEntry st)
            ]
        ]
      , HH.div
          [ HP.class_ (HH.ClassName "container") ]
          [ HH.div
              [ HP.class_ (HH.ClassName "row") ]
              -- always include all children, even if not visible (d-none)
              -- since avoids reinitialising the elements (e.g. Test would loose current position if go to configure, and cancel)
              [ HH.div
                  [ HP.class_ (HH.ClassName $ "col-12 " <> visibility.vocab) ]
                  [ HH.slot' CP.cp1 unit VocabSelector.component
                      { vocabFile : st.vocabFile }
                      (HE.input HandleVocabSelector)
                  ]
              , HH.div
                  [ HP.class_ (HH.ClassName $ "col-12 " <> visibility.group) ]
                  case st.vocab of
                    Just vocab → [ HH.slot' CP.cp2 unit GroupSelector.component
                                     { vocab  : vocab
                                     , groups : st.groups
                                     }
                                     (HE.input HandleGroupSelector)
                                 ]
                    Nothing    → []
              , HH.div
                  [ HP.class_ (HH.ClassName $ "col-12 " <> visibility.configure) ]
                  [ HH.slot' CP.cp3 unit Configure.component
                     { configuration : st.configuration }
                     (HE.input HandleConfigure)
                  ]
              , HH.div
                  [ HP.class_ (HH.ClassName $ "col-12 " <> visibility.test) ]
                  case st.vocab of
                    Just vocab → [ HH.slot' CP.cp4 unit Test.component
                                     { vocab         : vocab
                                     , groups        : st.groups
                                     , configuration : st.configuration
                                     }
                                     (const Nothing)
                                 ]
                    Nothing    → []
              ]
          ]
      ]
    where
      collapseId = "navbarToggleContent"
      visibility = case Tuple st.mode st.vocab of
        Tuple SelectVocab _            → { vocab: ""      , group: "d-none", configure: "d-none", test: "d-none"}
        Tuple SelectGroup (Just vocab) → { vocab: "d-none", group: ""      , configure: "d-none", test: "d-none"}
        Tuple Configure   _            → { vocab: "d-none", group: "d-none", configure: ""      , test: "d-none"}
        Tuple Test        (Just vocab) → { vocab: "d-none", group: "d-none", configure: "d-none", test: ""      }
        Tuple _           Nothing      → { vocab: ""      , group: "d-none", configure: "d-none", test: "d-none"}

      modes ∷ Array Mode
      modes = enumFromTo bottom top


  navEntry :: State -> Mode -> HTML
  navEntry st mode = HH.li
      [ HP.class_ (HH.ClassName $ "nav-item " <> if A.elem mode [Test, SelectGroup] && isNothing st.vocab then "disabled" -- these groups require Vocab to have been loaded
                                                 else if st.mode == mode then "active"
                                                 else ""
                  )
      ]
      [ HH.a
          [ HP.class_ (HH.ClassName "nav-link")
          , HE.onClick (HE.input_ (SetMode mode))
          ]
          [ HH.text (show mode) ]
      ]


  eval ∷ Query ~> DSL
  eval (Initialize next) = do
    H.liftEffect restoreMode >>= case _ of
      Left err   → H.liftEffect $ error $ "Could not restore mode: " <> err
      Right mode → do H.liftEffect $ log $ "restoring mode " <> show mode
                      H.modify_ _ { mode = mode }

    H.liftEffect restoreConfiguration >>= case _ of
      Left err            → H.liftEffect $ error $ "Could not restore configuration: " <> err
      Right configuration → H.modify_ _ { configuration = configuration }

    H.liftEffect restoreVocabFile >>= case _ of
      Left err        → H.liftEffect $ error $ "Could not restore vocabFile: " <> err
      Right vocabFile → do vocab  ← H.liftAff $ loadVocab vocabFile
                           groups ← H.liftEffect restoreGroups
                           H.modify_ _ { vocabFile = Just vocabFile
                                       , vocab     = Just vocab
                                       , groups    = either (const []) identity groups
                                       }
    pure next

  eval (SetMode mode next) = H.modify_ _ { mode = mode } $> next
  eval (HandleVocabSelector (VocabSelector.NotifyVocabFile Nothing) next) = do
    H.modify_ _ { mode = Test } $> next
  eval (HandleVocabSelector (VocabSelector.NotifyVocabFile (Just vocabFile)) next) = do
    vocab ← H.liftAff $ loadVocab vocabFile

    let mode   = Test
        groups = vGroups vocab

    -- TODO can we just back the State with local storage?
    H.liftEffect $ storeVocabFile vocabFile
    H.liftEffect $ storeGroups groups
    H.liftEffect $ storeMode mode

    H.modify_ $ merge { vocabFile : Just vocabFile
                      , vocab     : Just vocab
                      , groups
                      , mode
                      }
    pure next
  eval (HandleGroupSelector (GroupSelector.NotifyGroups groups) next) = do
    H.modify_ _ { groups = groups
                , mode   = Test
                } $> next
  eval (HandleConfigure (Configure.NotifyConfiguration configuration) next) = do
    H.liftEffect $ storeConfiguration configuration
    H.modify_ _ { configuration = configuration
                , mode          = Test
                } $> next

dataToggle ∷ ∀ r i. String → HP.IProp r i
dataToggle = HP.attr (HH.AttrName "data-toggle")

dataTarget ∷ ∀ r i. String → HP.IProp r i
dataTarget = HP.attr (HH.AttrName "data-target")
