module Ulpan.Component.Test where

import Prelude

import Data.Array as A
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
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
import Ulpan.Model (Configuration, TestDirection(..), TestOrdering(..), Vocab, VocabEntry)
import Ulpan.Util (shuffle)


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
  | ShowAnswer a

type Input =
  { vocab         ∷ Vocab
  , configuration ∷ Configuration
  }

type Output = Void

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
    , configuration : input.configuration
    -- following are default values, to be replaced after initialise
    , fromL1        : true
    , index         : FileOrderedIndex { length : 0, current : 0 }
    , current       : unsafePartial fromJust $ A.head (lfVocab input.vocab) -- TODO do we know list is not empty? If so, make NonEmptyList?
    , showAnswer    : false
    }
  render ∷ State → H.ComponentHTML Query
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "container-fluid") ]
    $ [ HH.h3
        [ HP.class_ (HH.ClassName "text-center") ]
        [ HH.text (unwrap (unwrap st.vocab).meta).name ]
      , HH.h4
        [ HP.class_ (HH.ClassName "text-center") ]
        [ HH.text ("(" <> show st.index <> ")") ]
       , HH.table
        [ HP.class_ (HH.ClassName "test-table") ]
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
            [ HP.class_ (HH.ClassName $ "textarea " <> answerWrapperClass) ]
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
        ] <> (if (unwrap st.configuration).showNotes then
               [ HH.tr_
                 [ HH.td
                   [ HP.class_ (HH.ClassName $ "textarea " <> notesWrapperClass) ]
                   [ HH.div
                     [ HP.class_ (HH.ClassName notesClass) ]
                     [ HH.text notes ]
                   ]
                 ]]
             else []
             )
          <>
        [ HH.tr_
          [ HH.td_
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
                    , HE.onClick (HE.input_ ShowAnswer)
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
      question           = if st.fromL1 then
                             unwrap (unwrap st.current).language1
                           else
                             unwrap (unwrap st.current).language2
      answer             = if st.fromL1 then
                             unwrap (unwrap st.current).language2
                           else
                             unwrap (unwrap st.current).language1
      notes              = (unwrap st.current).note
      questionClass      = if st.fromL1 then
                             show (unwrap (unwrap st.vocab).meta).language1Gravity
                             <> " " <>
                             show (unwrap (unwrap st.vocab).meta).language1TextSize
                           else
                             show (unwrap (unwrap st.vocab).meta).language2Gravity
                             <> " " <>
                             show (unwrap (unwrap st.vocab).meta).language2TextSize
      answerWrapperClass = if not st.showAnswer then "lightgrey" else ""
      answerClass        = if not st.showAnswer then
                             "d-none "
                           else if st.fromL1 then
                             show (unwrap (unwrap st.vocab).meta).language2Gravity
                             <> " " <>
                             show (unwrap (unwrap st.vocab).meta).language2TextSize
                           else
                             show (unwrap (unwrap st.vocab).meta).language1Gravity
                             <> " " <>
                             show (unwrap (unwrap st.vocab).meta).language1TextSize
      notesWrapperClass  = if not st.showAnswer then "lightgrey" else ""
      notesClass         = if not st.showAnswer then "d-none" else ""


  eval ∷ Query ~> DSL
  eval (Initialize next) = do
    st ← H.get
    initFromL1 st.configuration
    initIndex st.configuration st.vocab
    pure next

  eval (HandleInput input next) = do
    st ← H.get
    H.modify_ (merge input)
    when ((unwrap input.configuration).testDirection /= (unwrap st.configuration).testDirection) $
      initFromL1 input.configuration
    when (  input.vocab /= st.vocab
         || (unwrap input.configuration).testOrdering /= (unwrap st.configuration).testOrdering
         ) $
      initIndex input.configuration input.vocab
    pure next

  eval (ShowAnswer next) = do
    H.modify_ _ { showAnswer = true }
    pure next

  eval (Next next) = do
    vocabEntries ← H.gets _.vocab <#> lfVocab
    index        ← H.gets _.index
    updateIndex vocabEntries index
    pure next

  initFromL1 configuration = do
    fromL1 ← case (unwrap configuration).testDirection of
               FromLanguage1 → pure true
               FromLanguage2 → pure false
               DShuffled     → H.liftEffect randomBool
    H.modify_ $ merge { fromL1 }

  initIndex configuration vocab = do
    let vocabEntries = lfVocab vocab
    index  ← H.liftEffect $ mkIdx (unwrap configuration).testOrdering (A.length vocabEntries)
    updateIndex vocabEntries index

  updateIndex vocabEntries index = do
    Tuple i index' ← H.liftEffect $ getNextIndex index
    H.modify_ $ merge { index      : index'
                      , current    : unsafePartial fromJust $ vocabEntries A.!! i -- can we prove i is in range?
                      , showAnswer : false
                      }


mkIdx ∷ TestOrdering → Int → Effect Index
mkIdx FileOrdered length =
  pure $ FileOrderedIndex { length, current: 0 }
mkIdx Shuffled length =
  shuffle (A.range 0 length) <#> \shuffledIndices → ShuffledIndex { length, current: 0, shuffledIndices }
mkIdx Random length =
  pure (RandomIndex { length })

getNextIndex ∷ Index → Effect (Tuple Int Index)
getNextIndex (FileOrderedIndex { length,  current }) = do
  let next = current + 1 `mod` length
  pure $ Tuple next $ FileOrderedIndex { length
                                       , current : next
                                       }
getNextIndex (ShuffledIndex { length, current, shuffledIndices }) =
  case A.uncons shuffledIndices of
    Nothing            → mkIdx Shuffled length >>= getNextIndex
    Just { head, tail} → pure $ Tuple head $ ShuffledIndex { length
                                                           , current         : current + 1
                                                           , shuffledIndices : tail
                                                           }
getNextIndex (RandomIndex { length }) = do
  next ← randomInt 0 length
  pure $ Tuple next $ RandomIndex { length }


lfVocab ∷ Vocab → Array VocabEntry
lfVocab vocab =
  A.concat $ A.fromFoldable $ M.values (unwrap vocab).vocab
