module HalogenUtil where

import Halogen.HTML.Events (EventProp, MouseEvent)
import Halogen.HTML.Core (handler, eventName)


onTouchEnd :: forall i. EventProp MouseEvent i
onTouchEnd = handler (eventName "touchend")
