module Ulpan.Util where

import Effect (Effect)

foreign import shuffle
  ∷ ∀ a. Array a → Effect (Array a)
