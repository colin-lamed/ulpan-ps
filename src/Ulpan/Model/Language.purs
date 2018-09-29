module Ulpan.Model.Language where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Argonaut.Decode (class DecodeJson, getField)
import Data.Array as A
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Map (Map)
import Data.Newtype (class Newtype, unwrap)
import Data.TraversableWithIndex (traverseWithIndex)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Object as O
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)


data Gravity = GRight | GCentre | GLeft

derive instance eqGravity ∷ Eq Gravity

derive instance genericGravity ∷ Generic Gravity _

instance showGravity ∷ Show Gravity where
  show = genericShow

instance decodeJsonGravity ∷ DecodeJson Gravity where
  decodeJson json = do
    s ← note "Gravity not a string" (toString json)
    case s of
      "right"  → pure GRight
      "centre" → pure GCentre
      _        → pure GLeft


data TextSize = Small | Large | XLarge | Medium

derive instance eqTextSize ∷ Eq TextSize

derive instance genericTextSize ∷ Generic TextSize _

instance showTextSize ∷ Show TextSize where
  show = genericShow

instance decodeJsonTextSize ∷ DecodeJson TextSize where
  decodeJson json = do
    s ← note "TextSize not a string" (toString json)
    case s of
      "small"  → pure Small
      "large"  → pure Large
      "xlarge" → pure XLarge
      _        → pure Medium


newtype Language = Language String

derive instance newtypeLanguage ∷ Newtype Language _

derive instance eqLanguage ∷ Eq Language

derive instance genericLanguage ∷ Generic Language _

instance showLanguage ∷ Show Language where
  show = genericShow

instance decodeJsonLanguage ∷ DecodeJson Language where
  decodeJson json = do
    s ← note "Language not a string" (toString json)
    pure $ Language s

newtype VocabEntry = VocabEntry
  { language1 ∷ Language
  , language2 ∷ Language
  , note      ∷  String
  }

derive instance newtypeVocabEntry ∷ Newtype VocabEntry _

derive instance eqVocabEntry ∷ Eq VocabEntry

derive instance genericVocabEntry ∷ Generic VocabEntry _

instance showVocabEntry ∷ Show VocabEntry where
  show = genericShow

decodeVocabEntry ∷ Meta → Int → Json → Either String VocabEntry
decodeVocabEntry meta i json = do
    o         ← note ("VocabEntry " <> show i <> " not an object") (toObject json)
    language1 ← getField o ((unwrap meta).language1Key)
    language2 ← getField o ((unwrap meta).language2Key)
    note      ← getField o ((unwrap meta).noteKey) <|> pure ""
    pure $ VocabEntry
      { language1
      , language2
      , note
      }

decodeVocabEntries ∷ Meta → Json → Either String (Array VocabEntry)
decodeVocabEntries meta json = do
    a ← note "VocabEntries not an Array" (toArray json)
    traverseWithIndex (decodeVocabEntry meta) a

newtype Group = Group String

derive instance newtypeGroup ∷ Newtype Group _

derive instance eqGroup ∷ Eq Group

derive instance ordGroup ∷ Ord Group

derive instance genericGroup ∷ Generic Group _

instance showGroup ∷ Show Group where
  show = genericShow

instance decodeJsonGroup ∷ DecodeJson Group where
  decodeJson json = do
    map Group $ note "Group not a string" (toString json)


newtype Meta = Meta
  { name              ∷ String
  , language1Key      ∷ String
  , language2Key      ∷ String
  , noteKey           ∷ String
  , language1Gravity  ∷ Gravity
  , language2Gravity  ∷ Gravity
  , language1TextSize ∷ TextSize
  , language2TextSize ∷ TextSize
  }

derive instance newtypeMeta ∷ Newtype Meta _

derive instance eqMeta ∷ Eq Meta

derive instance genericMeta ∷ Generic Meta _

instance showMeta ∷ Show Meta where
  show = genericShow

instance decodeJsonMeta ∷ DecodeJson Meta where
  decodeJson json = do
    o                 ← note "Meta not an object" (toObject json)
    name              ← getField o "name"
    language1Key      ← getField o "language1"
    language2Key      ← getField o "language2"
    noteKey           ← getField o "note"
    language1Gravity  ← getField o "language1_gravity" <|> pure GLeft
    language2Gravity  ← getField o "language2_gravity" <|> pure GLeft
    language1TextSize ← getField o "language1_textsize" <|> pure Medium
    language2TextSize ← getField o "language2_textsize" <|> pure Medium
    pure $ Meta
      { name
      , language1Key
      , language2Key
      , noteKey
      , language1Gravity
      , language2Gravity
      , language1TextSize
      , language2TextSize
      }

newtype Vocab = Vocab
  { meta  ∷ Meta
  , vocab ∷ Map Group (Array VocabEntry)
  }

derive instance newtypeVocab ∷ Newtype Vocab _

derive instance genericVocab ∷ Generic Vocab _

derive instance eqVocab ∷ Eq Vocab

instance showVocab ∷ Show Vocab where
  show = genericShow

instance decodeJsonVocab ∷ DecodeJson Vocab where
  decodeJson json = do
    o     ← note "Vocab not an object" (toObject json)
    meta  ← getField o "meta"
    vocab ← let d ∷ O.Object Json
                d = O.filterKeys (_ /= "meta") o
                combine a k v = decodeVocabEntries meta v <#> \vocabEntries → M.insert (Group k) vocabEntries a
            in O.foldM combine M.empty d
    pure $ Vocab
      { meta
      , vocab
      }

vGroups :: Vocab -> Array Group
vGroups vocab = A.fromFoldable $ M.keys (unwrap vocab).vocab

-- VocabFile

newtype VocabFile = VocabFile
  { name ∷ String
  , url  ∷ String
  }

derive instance eqVocabFile ∷ Eq VocabFile

derive instance newtypeVocabFile ∷ Newtype VocabFile _

derive instance genericVocabFile ∷ Generic VocabFile _

instance showVocabFile ∷ Show VocabFile where
  show = genericShow

instance genericDecodeVocabFile ∷ Decode VocabFile where
  decode = genericDecode defaultOptions

instance arbitraryVocabFile ∷ Arbitrary VocabFile where
  arbitrary = genericArbitrary

instance decodeJsonVocabFile ∷ DecodeJson VocabFile where
  decodeJson json = do
    o    ← note "VocabFile not an object" (toObject json)
    name ← getField o "name"
    url  ← getField o "url"
    pure $ VocabFile
      { name
      , url
      }
