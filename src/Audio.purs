module Audio where

import Prelude
import DOM.File.Types (Blob)
import Data.ArrayBuffer.Types (ArrayBuffer)


foreign import initContext :: Int -> Blob

foreign import play :: Blob -> ArrayBuffer -> Unit
