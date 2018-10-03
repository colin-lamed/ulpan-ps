module Ulpan.Component.Swipe where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Web.HTML.HTMLElement (HTMLElement)

foreign import _swipe
  ∷ { threshold :: Int } → HTMLElement → (EffectFn1 Boolean Unit) → Effect Unit

swipe
  ∷ { threshold :: Int } → HTMLElement → (SwipeDir → Effect Unit) → Effect Unit
swipe settings elm ef1 = _swipe settings elm (mkEffectFn1 \b -> ef1 if b then SDLeft else SDRight)

data SwipeDir = SDLeft | SDRight

derive instance eqSwipeDir ∷ Eq SwipeDir
