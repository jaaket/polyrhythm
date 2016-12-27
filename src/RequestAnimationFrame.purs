module RequestAnimationFrame where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import DOM

foreign import requestAnimationFrame :: forall a eff. (Number -> Aff (dom :: DOM | eff) a) -> Aff (dom :: DOM | eff) Unit
