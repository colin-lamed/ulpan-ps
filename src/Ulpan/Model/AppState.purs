module Ulpan.Model.AppState where

import Prelude
import Data.Enum (class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericSucc, genericPred)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Mode = SelectVocab | SelectGroup | Configure | Test

derive instance eqMode ∷ Eq Mode

derive instance ordMode ∷ Ord Mode

derive instance genericMode ∷ Generic Mode _

instance showMode ∷ Show Mode where
  show SelectVocab = "Select Vocab"
  show SelectGroup = "Select Group"
  show Configure   = "Configure"
  show Test        = "Test"


instance boundedMode ∷ Bounded Mode where
  top    = genericTop
  bottom = genericBottom

instance enumMode ∷ Enum Mode where
  succ = genericSucc
  pred = genericPred

instance arbitraryMode ∷ Arbitrary Mode where
  arbitrary = genericArbitrary
