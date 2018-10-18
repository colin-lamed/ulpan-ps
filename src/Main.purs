module Main where

import Prelude
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Console (warn)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Ulpan.Component.App as App
import Data.ServiceWorker (registerServiceWorker)

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  H.liftAff $ (attempt $ registerServiceWorker "service-worker.js") >>=
    either (\e → H.liftEffect $ warn $ "Could not register service worker: " <> show e) pure
  void $ runUI App.component unit body
