module Ulpan.LocalStorage where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Int (fromString)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String.Common as S
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON)
import Ulpan.Model (Configuration, Group(..), Mode, VocabFile)


foreign import setLocalStorage
  ∷ String → String → Effect Unit

foreign import jsGetLocalStorage
  ∷ String → Effect (Nullable String)

getLocalStorage ∷ String → Effect (Maybe String)
getLocalStorage k = jsGetLocalStorage k >>= toMaybe >>> pure

encodeMode ∷ Mode → String
encodeMode = genericEncodeJSON defaultOptions

decodeMode ∷ String → Either String Mode
decodeMode = lmap show <<< runExcept <<< genericDecodeJSON defaultOptions

encodeVocabFile ∷ VocabFile → String
encodeVocabFile = genericEncodeJSON defaultOptions

decodeVocabFile ∷ String → Either String VocabFile
decodeVocabFile = lmap show <<< runExcept <<< genericDecodeJSON defaultOptions

encodeConfiguration ∷ Configuration → String
encodeConfiguration = genericEncodeJSON defaultOptions

decodeConfiguration ∷ String → Either String Configuration
decodeConfiguration = lmap show <<< runExcept <<< genericDecodeJSON defaultOptions

storeConfiguration ∷ Configuration → Effect Unit
storeConfiguration = encodeConfiguration >>> setLocalStorage "configuration"

restoreConfiguration ∷ Effect (Either String Configuration)
restoreConfiguration =
  getLocalStorage "configuration"
    >>= maybe (Left "No configuration") decodeConfiguration
    >>> pure

storeMode ∷ Mode → Effect Unit
storeMode = encodeMode >>> setLocalStorage "mode"

restoreMode ∷ Effect (Either String Mode)
restoreMode =
  getLocalStorage "mode"
    >>= maybe (Left "No mode") decodeMode
    >>> pure

storeVocabFile ∷ VocabFile → Effect Unit
storeVocabFile = encodeVocabFile >>> setLocalStorage "vocabFile"

restoreVocabFile ∷ Effect (Either String VocabFile)
restoreVocabFile =
  getLocalStorage "vocabFile"
    >>= maybe (Left "No vocabFile") decodeVocabFile
    >>> pure

storeIndex ∷ Int → Effect Unit
storeIndex = show >>> setLocalStorage "index"

restoreIndex ∷ Effect (Either String Int)
restoreIndex =
  getLocalStorage "index"
    >>= (_ >>= fromString) >>> pure
    >>= maybe (Left "No index") Right
    >>> pure


-- TODO store as json (separator may already be contained in string)
storeGroups :: Array Group -> Effect Unit
storeGroups = map unwrap >>> intercalate "$$" >>> setLocalStorage "groups"

restoreGroups :: Effect (Either String (Array Group))
restoreGroups =
  getLocalStorage "groups"
    >>= (_ >>= S.split (Pattern ",") >>> map Group >>> pure) >>> pure
    >>= maybe (Left "No groups") Right
    >>> pure
