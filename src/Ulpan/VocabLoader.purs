module Ulpan.VocabLoader
  ( loadAvailableVocabs
  , loadVocab
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(GET))
import Data.Newtype (unwrap)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Effect.Aff (Aff, throwError)
import Ulpan.Model (Vocab, VocabFile)
import Effect.Exception as Ex
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXRs
import Network.HTTP.StatusCode(StatusCode(StatusCode))

loadAvailableVocabs ∷ Aff (Array VocabFile)
loadAvailableVocabs = do
  jsonStr ← get "resources/availableVocabs.json"
  case jsonParser jsonStr >>= decodeJson of
    Left err  → throwError $ Ex.error $ "Could not parse json: " <> show err
    Right res → pure res

loadVocab ∷ VocabFile → Aff Vocab
loadVocab vocabFile = do
  yamlStr ← get (unwrap vocabFile).url
  case runExcept $ parseYAMLToJson yamlStr of
    Left err   → throwError $ Ex.error $ "Could not parse yaml: " <> show err
    Right json → either (throwError <<< Ex.error) pure (decodeJson json)

get ∷ String → Aff String
get url = do
  let req = AX.defaultRequest { url    = url
                              , method = Left GET
                              }
  res ← AX.affjax AXRs.string req
  case res.status of
      StatusCode 0   → pure res.response -- for local file access
      StatusCode 200 → pure res.response
      StatusCode sc  → throwError $ Ex.error $ "Failed to get " <> url <> " - status code: " <> show sc
