module Ulpan.Util where

import Effect (Effect)

foreign import shuffle
  ∷ ∀ a. String → Array a → Effect (Array a)
