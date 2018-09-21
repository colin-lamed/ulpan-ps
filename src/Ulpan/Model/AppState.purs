module Ulpan.Model.AppState where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Mode = SelectVocab | Configure | Test

derive instance eqMode ∷ Eq Mode

derive instance genericMode ∷ Generic Mode _

instance showMode ∷ Show Mode where
  show = genericShow

instance arbitraryMode ∷ Arbitrary Mode where
  arbitrary = genericArbitrary
