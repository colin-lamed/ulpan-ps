module Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Ulpan.Component.App as App
import Data.ServiceWorker (registerServiceWorker)

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  H.liftAff $ registerServiceWorker "service-worker.js"
  void $ runUI App.component unit body
