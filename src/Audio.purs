module Audio where

import Prelude
import DOM.File.Types (Blob)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Control.Monad.Eff


foreign import data AUDIO :: !

foreign import play :: forall e. ArrayBuffer -> Eff (audio :: AUDIO | e) Unit
