module Data.ServiceWorker where

import Prelude
import Effect (Effect)

foreign import registerServiceWorker ∷ String → Effect Unit
