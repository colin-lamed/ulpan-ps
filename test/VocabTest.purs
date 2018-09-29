module Test.VocabTest
  ( vocabTests
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Effect.Class (liftEffect)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestSuite, failure, success, test)


testGetVocab :: String -> TestSuite
testGetVocab filename = do
  test ("get " <> filename) do
    yamlStr ← liftEffect $ readTextFile UTF8 ("public/resources/vocab/" <> filename)
    case runExcept $ parseYAMLToJson yamlStr of
      Left err   → failure $ "Could not parse yaml: " <> show err
      Right json → success


vocabTests :: TestSuite
vocabTests = do
  traverse_ testGetVocab [ "ulpan.yml"
                         , "hsk1.yml"
                         , "hebrew_colloquial_course.yml"
                         ]
