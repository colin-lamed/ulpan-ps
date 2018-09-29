module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Test.LocalStorageTest (localStorageTests)
import Test.VocabTest (vocabTests)

main ∷ Effect Unit
main = runTest do
  suite "all tests" do
    localStorageTests
    vocabTests
