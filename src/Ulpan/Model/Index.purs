module Ulpan.Model.Index where

import Prelude
import Data.Array as A
import Data.Maybe (fromJust)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Random (randomInt)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Partial.Unsafe (unsafePartial)
import Ulpan.Model (TestOrdering(..))
import Ulpan.Util (shuffle)

data Index = FileOrderedIndex { length          ∷ Int
                              , current         ∷ Int
                              }
           | ShuffledIndex    { length          ∷ Int
                              , current         ∷ Int
                              , seed            ∷ String
                              , shuffledIndices ∷ Array Int
                              }
           | RandomIndex      { length          ∷ Int
                              , current         ∷ Int
                              }

instance showIndex ∷ Show Index where
  show (FileOrderedIndex { length, current }) = "File Ordered (" <> show (current + 1) <> "/" <> show length <> ")"
  show (ShuffledIndex { length, current })    = "Shuffled ("     <> show (current + 1) <> "/" <> show length <> ")"
  show (RandomIndex _)                        = "Random"

derive instance genericTestOrdering ∷ Generic Index _

instance genericDecodeTestOrdering ∷ Decode Index where
  decode = genericDecode defaultOptions

instance genericEncodeTestOrdering ∷ Encode Index where
  encode = genericEncode defaultOptions


mkIndex ∷ TestOrdering → Int → Int → String → Effect Index
mkIndex FileOrdered length current _ =
  pure $ FileOrderedIndex { length, current }
mkIndex Shuffled length current seed = do
  shuffledIndices <- shuffle seed (A.range 0 length)
  pure $ ShuffledIndex { length, current, seed, shuffledIndices }
mkIndex Random length _ _ =
  pure $ RandomIndex { length, current: 0 }


getCurrent ∷ Index → Int
getCurrent (FileOrderedIndex { current }) = current
getCurrent (ShuffledIndex    { current, shuffledIndices }) = unsafePartial fromJust $ shuffledIndices A.!! current
getCurrent (RandomIndex      { current }) = current


advanceIndex ∷ Index → Effect Index
advanceIndex (FileOrderedIndex { length,  current }) =
  pure $ FileOrderedIndex { length
                          , current : current + 1 `mod` length
                          }
advanceIndex (ShuffledIndex { length, current, seed, shuffledIndices })
  | current + 1 == length = do
      newSeed ← randomInt 0 1000 >>= show >>> pure
      mkIndex Shuffled length 0 newSeed
  | otherwise = do
      pure $ ShuffledIndex { length
                           , current         : current + 1
                           , seed
                           , shuffledIndices
                           }
advanceIndex (RandomIndex { length, current }) = do
  next ← randomInt 0 length
  pure $ RandomIndex { length, current : next }
