module Audio where

import Prelude
import DOM.File.Types (Blob)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Control.Monad.Eff
import Control.Monad.Aff


foreign import data AUDIO :: !

foreign import data Sample :: *

foreign import loadSample_ :: forall e. (Sample -> Eff (audio :: AUDIO | e) Unit) -> ArrayBuffer -> Eff (audio :: AUDIO | e) Unit

loadSample :: forall e. ArrayBuffer -> Aff (audio :: AUDIO | e) Sample
loadSample buffer = makeAff (\error success -> loadSample_ success buffer)

foreign import play :: forall e. Sample -> Eff (audio :: AUDIO | e) Unit
