module Ulpan.Util where

import Prelude
import Effect (Effect)

foreign import shuffle
  ∷ forall a. Array a → Effect (Array a)
