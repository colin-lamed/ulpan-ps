module Ulpan.Component.Test where

import Prelude

import Data.Array as A
import Data.Either (hush)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed.ButtonType (ButtonType(ButtonButton))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Random (randomBool, randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Record (merge)
import Ulpan.LocalStorage (storeIndex, restoreIndex)
import Ulpan.Model (Configuration, Group, TestDirection(..), TestOrdering(..), Vocab, VocabEntry, (^))
import Ulpan.Util (shuffle)
import Ulpan.Component.Swipe (swipe, SwipeDir(..))



data Index = FileOrderedIndex { length          ∷ Int
                              , current         ∷ Int
                              }
           | ShuffledIndex    { length          ∷ Int
                              , current         ∷ Int
                              , shuffledIndices ∷ Array Int
                              }
           | RandomIndex      { length          ∷ Int
                              }

instance showIndex ∷ Show Index where
  show (FileOrderedIndex { length, current }) = "File Ordered (" <> show current <> "/" <> show length <> ")"
  show (ShuffledIndex { length, current })    = "Shuffled (" <> show current <> "/" <> show length <> ")"
  show (RandomIndex _)                        = "Random"


type State =
  { vocab         ∷ Vocab
  , groups        ∷ Array Group
  , configuration ∷ Configuration
  , fromL1        ∷ Boolean -- ^ based on configuration.testDirection - resolving Random to a particular value
  , index         ∷ Index
  , current       ∷ VocabEntry
  , showAnswer    ∷ Boolean
  }

data Query a
  = Initialize a
  | HandleInput Input a
  | Next a
  | ToggleShowAnswer a
  | HandleSwipeEvent SwipeDir (H.SubscribeStatus -> a)

type Input =
  { vocab         ∷ Vocab
  , groups        ∷ Array Group
  , configuration ∷ Configuration
  }

data Output =
  NotifyIndex Int

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
  initialState input =
    { vocab         : input.vocab
    , groups        : input.groups
    , configuration : input.configuration
    -- following are default values, to be replaced after initialise
    , fromL1        : true
    , index         : FileOrderedIndex { length : 0, current : 0 }
    , current       : unsafePartial fromJust $ A.head (lfVocab input.vocab input.groups) -- TODO do we know list is not empty? If so, make NonEmptyList?
    , showAnswer    : false
    }
  render ∷ State → H.ComponentHTML Query
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "container-fluid") ]
    $ [ HH.h3
        [ HP.class_ (HH.ClassName "text-center") ]
        [ HH.text $ st.vocab ^ _.meta ^ _.name ]
      , HH.h4
        [ HP.class_ (HH.ClassName "text-center") ]
        [ HH.text $ show st.index ]
       , HH.table
        [ HP.ref (H.RefLabel "test-table")
        , HP.class_ (HH.ClassName "test-table")
        ]
      $ [ HH.tr_
          [ HH.td
            [ HP.class_ (HH.ClassName $ "textarea") ]
            [ HH.div
              [ HP.class_ (HH.ClassName $ questionClass) ]
              [ HH.text question ]
            ]
          ]
        , HH.tr_
          [ HH.td
            [ HP.class_ (HH.ClassName "spacer") ]
            [ HH.text ""]
          ]
        , HH.tr_
          [ HH.td
            [ HP.class_ (HH.ClassName $ "textarea " <> answerWrapperClass)
            , HE.onClick (HE.input_ ToggleShowAnswer)
            ]
            [ HH.div
                [ HP.class_ (HH.ClassName answerClass) ]
                [ HH.text answer]
            ]
          ]
        , HH.tr_
          [ HH.td
            [ HP.class_ (HH.ClassName "spacer") ]
            [ HH.text ""]
          ]
        ] <> (if st.configuration ^ _.showNotes then
               [ HH.tr_
                 [ HH.td
                   [ HP.class_ (HH.ClassName $ "textarea " <> notesWrapperClass)
                   , HE.onClick (HE.input_ ToggleShowAnswer)
                   ]
                   [ HH.div
                     [ HP.class_ (HH.ClassName notesClass) ]
                     [ HH.text notes ]
                   ]
                 ]]
             else []
             )
          <>
        [ HH.tr_
          [ HH.td
            [ HP.class_ (HH.ClassName "spacer") ]
            [ HH.text ""]
          ]
        , HH.tr_
          [ HH.td
            [ HP.class_ (HH.ClassName "text-center") ]
            [ HH.div
              [ HP.class_ (HH.ClassName "btn-group")  ]
              (if st.showAnswer
                then
                  [ HH.button
                    [ HP.type_ ButtonButton
                    , HP.class_ (HH.ClassName "btn btn-primary")
                    , HE.onClick (HE.input_ Next)
                    ]
                    [ HH.span
                      [ HP.class_ (HH.ClassName "fa fa-arrow-circle-right") ]
                      [ HH.text " Next"]
                    ]
                  ]
                else
                  [ HH.button
                    [ HP.type_ ButtonButton
                    , HP.class_ (HH.ClassName "btn btn-primary")
                    , HE.onClick (HE.input_ ToggleShowAnswer)
                    ]
                    [ HH.span
                      [ HP.class_ (HH.ClassName "fa fa-eye") ]
                      [ HH.text " Show"]
                    ]
                  ]
              )
            ]
          ]
        ]
      ]
    where
      question           = unwrap if st.fromL1 then
                             st.current ^ _.language1
                           else
                             st.current ^ _.language2
      answer             = unwrap if st.fromL1 then
                             st.current ^ _.language2
                           else
                             st.current ^ _.language1
      notes              = st.current ^ _.note
      questionClass      = if st.fromL1 then
                             show (st.vocab ^ _.meta ^ _.language1Gravity)
                             <> " " <>
                             show (st.vocab ^ _.meta ^ _.language1TextSize)
                           else
                             show (st.vocab ^ _.meta ^ _.language2Gravity)
                             <> " " <>
                             show (st.vocab ^ _.meta ^ _.language2TextSize)
      answerWrapperClass = if not st.showAnswer then "lightgrey" else ""
      answerClass        = if not st.showAnswer then
                             "d-none "
                           else if st.fromL1 then
                             show (st.vocab ^ _.meta ^ _.language2Gravity)
                             <> " " <>
                             show (st.vocab ^ _.meta ^ _.language2TextSize)
                           else
                             show (st.vocab ^ _.meta ^ _.language1Gravity)
                             <> " " <>
                             show (st.vocab ^ _.meta ^ _.language1TextSize)
      notesWrapperClass  = if not st.showAnswer then "lightgrey" else ""
      notesClass         = if not st.showAnswer then "d-none" else ""


  eval ∷ Query ~> DSL
  eval (Initialize next) = do
    st ← H.get
    initFromL1 st.configuration
    initIndex st.configuration st.vocab st.groups

    H.getHTMLElementRef (H.RefLabel "test-table") >>= case _ of
      Nothing  -> pure unit
      Just elm -> H.subscribe $ H.eventSource (swipe { threshold : 50 } elm) (Just <<< H.request <<< HandleSwipeEvent)

    pure next

  eval (HandleInput input next) = do
    st ← H.get
    H.modify_ (merge input)
    when (input.configuration ^ _.testDirection /= st.configuration ^ _.testDirection) $
      initFromL1 input.configuration
    when (  input.vocab /= st.vocab
         || input.groups /= st.groups
         || input.configuration ^ _.testOrdering /= st.configuration ^ _.testOrdering
         ) $ do
      H.liftEffect $ storeIndex 0
      initIndex input.configuration input.vocab input.groups
    pure next

  eval (ToggleShowAnswer next) = do
    H.modify_ \st -> st { showAnswer = not st.showAnswer }
    pure next

  eval (Next next) = advanceIndex $> next

  eval (HandleSwipeEvent dir reply) = do
    when (dir == SDLeft) advanceIndex
    pure (reply H.Listening)

  initFromL1 configuration = do
    fromL1 ← case configuration ^ _.testDirection of
               FromLanguage1 → pure true
               FromLanguage2 → pure false
               DShuffled     → H.liftEffect randomBool
    H.modify_ $ merge { fromL1 }

  initIndex configuration vocab groups = do
    let vocabEntries = lfVocab vocab groups
    current ← H.liftEffect $ restoreIndex
    index  ← H.liftEffect $ mkIdx (configuration ^ _.testOrdering) (A.length vocabEntries) (fromMaybe 0 $ hush current)
    updateIndex vocabEntries index

  updateIndex vocabEntries index = do
    Tuple i index' ← H.liftEffect $ getNextIndex index
    H.modify_ $ merge { index      : index'
                      , current    : unsafePartial fromJust $ vocabEntries A.!! i -- can we prove i is in range?
                      , showAnswer : false
                      }
    H.liftEffect $ storeIndex i

  advanceIndex = do
    st ← H.get
    let vocabEntries = lfVocab st.vocab st.groups
    updateIndex vocabEntries st.index

mkIdx ∷ TestOrdering → Int → Int → Effect Index
mkIdx FileOrdered length current =
  pure $ FileOrderedIndex { length, current }
mkIdx Shuffled length current =
  shuffle (A.range 0 length) <#> \shuffledIndices → ShuffledIndex { length, current, shuffledIndices }
mkIdx Random length _ =
  pure (RandomIndex { length })

getNextIndex ∷ Index → Effect (Tuple Int Index)
getNextIndex (FileOrderedIndex { length,  current }) = do
  let next = current + 1 `mod` length
  pure $ Tuple next $ FileOrderedIndex { length
                                       , current : next
                                       }
getNextIndex (ShuffledIndex { length, current, shuffledIndices }) =
  case A.uncons shuffledIndices of
    Nothing            → mkIdx Shuffled length 0 >>= getNextIndex
    Just { head, tail} → pure $ Tuple head $ ShuffledIndex { length
                                                           , current         : current + 1
                                                           , shuffledIndices : tail
                                                           }
getNextIndex (RandomIndex { length }) = do
  next ← randomInt 0 length
  pure $ Tuple next $ RandomIndex { length }


lfVocab ∷ Vocab → Array Group → Array VocabEntry
lfVocab vocab groups =
  if A.null filteredEntries then allEntries else filteredEntries
  where
    filteredEntries = A.concat $ A.fromFoldable $ M.values $ M.filterKeys (_ `A.elem`groups) $ vocab ^ _.vocab
    allEntries      = A.concat $ A.fromFoldable $ M.values $                                   vocab ^ _.vocab
