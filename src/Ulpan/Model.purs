module Ulpan.Model
  ( module Language
  , module Configuration
  , module AppState
  -- unwrap syntatic sugar
  , access, (^)
  )
 where

import Ulpan.Model.Language as Language
import Ulpan.Model.Configuration as Configuration
import Ulpan.Model.AppState as AppState
import Data.Newtype (class Newtype, unwrap)


access ∷ ∀ old new a. Newtype old new ⇒ old → (new → a) → a
access t f = f (unwrap t)
infixl 8 access as ^
