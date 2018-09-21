module Test.LocalStorageTest where

import Prelude

import Data.Either (Either(..))
import Test.QuickCheck (Result(), (===))
import Test.Unit (TestSuite, test)
import Test.Unit.QuickCheck (quickCheck)
import Partial.Unsafe (unsafeCrashWith)
import Ulpan.Model (Configuration, Mode, VocabFile)
import Ulpan.LocalStorage (decodeConfiguration, decodeMode, decodeVocabFile, encodeConfiguration, encodeMode, encodeVocabFile)


modeCanBeEncodedAndDecoded :: Mode -> Result
modeCanBeEncodedAndDecoded mode =
  case decodeMode $ encodeMode mode of
    Left err → unsafeCrashWith $ "Could not decode mode string: " <> show err -- TODO how to fail?
    Right mode' → mode === mode'

vocabFileCanBeEncodedAndDecoded :: VocabFile -> Result
vocabFileCanBeEncodedAndDecoded vocabFile =
  case decodeVocabFile $ encodeVocabFile vocabFile of
    Left err → unsafeCrashWith $ "Could not decode vocabFile string: " <> show err
    Right vocabFile' → vocabFile === vocabFile'

configurationCanBeEncodedAndDecoded :: Configuration -> Result
configurationCanBeEncodedAndDecoded configuration =
  case decodeConfiguration $ encodeConfiguration configuration of
    Left err → unsafeCrashWith $ "Could not decode configuration string: " <> show err
    Right configuration' → configuration === configuration'

localStorageTests :: TestSuite
localStorageTests = do
  test "mode can be encoded and decoded" $
    quickCheck modeCanBeEncodedAndDecoded
  test "vocabFile can be encoded and decoded" $
    quickCheck vocabFileCanBeEncodedAndDecoded
  test "configuration can be encoded and decoded" $
    quickCheck configurationCanBeEncodedAndDecoded
