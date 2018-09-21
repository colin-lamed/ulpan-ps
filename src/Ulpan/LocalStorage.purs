module Ulpan.LocalStorage where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON)
import Ulpan.Model (Configuration, Mode, Vocab, VocabFile)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, getField)
import Data.Argonaut.Parser (jsonParser)


foreign import setLocalStorage
  ∷ String → String → Effect Unit

foreign import jsGetLocalStorage
  ∷ String → Effect (Nullable String)

getLocalStorage ∷ String -> Effect (Maybe String)
getLocalStorage k = jsGetLocalStorage k >>= toMaybe >>> pure

encodeMode :: Mode -> String
encodeMode = genericEncodeJSON defaultOptions

decodeMode :: String -> Either String Mode
decodeMode = lmap show <<< runExcept <<< genericDecodeJSON defaultOptions

encodeVocabFile :: VocabFile -> String
encodeVocabFile = genericEncodeJSON defaultOptions

decodeVocabFile :: String -> Either String VocabFile
decodeVocabFile = lmap show <<< runExcept <<< genericDecodeJSON defaultOptions

encodeConfiguration :: Configuration -> String
encodeConfiguration = genericEncodeJSON defaultOptions

decodeConfiguration :: String -> Either String Configuration
decodeConfiguration = lmap show <<< runExcept <<< genericDecodeJSON defaultOptions


storeConfiguration :: Configuration -> Effect Unit
storeConfiguration = encodeConfiguration >>> setLocalStorage "configuration"

restoreConfiguration :: Effect (Either String Configuration)
restoreConfiguration = do
  getLocalStorage "configuration"
    >>= maybe (Left "No configuration") decodeConfiguration
    >>> pure

storeMode :: Mode -> Effect Unit
storeMode = encodeMode >>> setLocalStorage "mode"

restoreMode :: Effect (Either String Mode)
restoreMode = do
  getLocalStorage "mode"
    >>= maybe (Left "No mode") decodeMode
    >>> pure


storeVocabFile :: VocabFile -> Effect Unit
storeVocabFile = encodeVocabFile >>> setLocalStorage "vocabFile"

restoreVocabFile :: Effect (Either String VocabFile)
restoreVocabFile = do
  getLocalStorage "vocabFile"
    >>= maybe (Left "No vocabFile") decodeVocabFile
    >>> pure

-- Vocab using same Aeson defined for pulling from Apis
--
-- storeVocab :: Vocab -> Effect Unit
-- storeVocab = ?encodeVocab >>> setLocalStorage "vocab"
--
restoreVocab :: Effect (Either String Vocab)
restoreVocab = do
  getLocalStorage "vocab"
    >>= maybe (Left "No vocab") (\s -> jsonParser s >>= decodeJson)
    >>> pure
