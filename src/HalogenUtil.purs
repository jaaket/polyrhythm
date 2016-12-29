module HalogenUtil where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen (action)
import Halogen.HTML.Core (Prop, eventName, handler)
import Halogen.HTML.Events (EventProp, MouseEvent, input_, onMouseDown, preventDefault)
import Halogen.Query (Action)


onTouchStart :: forall i. EventProp MouseEvent i
onTouchStart = handler (eventName "touchstart")

onTouchEnd :: forall i. EventProp MouseEvent i
onTouchEnd = handler (eventName "touchend")

onMouseDownOrTouchStart :: forall f. Action f -> Array (Prop (f Unit))
onMouseDownOrTouchStart act =
  [ onMouseDown (input_ act)
  , onTouchStart \_ -> preventDefault $> Just (action act)
  ]
