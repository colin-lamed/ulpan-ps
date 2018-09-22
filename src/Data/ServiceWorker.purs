module Data.ServiceWorker
  ( registerServiceWorker
  ) where

import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import _registerServiceWorker ∷ String → EffectFnAff Unit

registerServiceWorker :: String -> Aff Unit
registerServiceWorker = fromEffectFnAff <<< _registerServiceWorker
