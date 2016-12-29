module Instrument where

import Prelude
import Data.Lens
import Audio
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Free (fromEff, fromAff)
import Data.Array (length, range, replicate)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import HalogenUtil (onMouseDownOrTouchStart)
import Network.HTTP.Affjax (get, AJAX)


type State = { sample :: Maybe Sample, notes :: Array Boolean, phase :: Int }

numNotes :: State -> Int
numNotes state = length state.notes

isNoteOn :: State -> Int -> Boolean
isNoteOn state note =
  case state.notes ^? ix note of
    Just true -> true
    _ -> false

data Query a
  = LoadSample String a
  | Tick a
  | ToggleNote Int a
  | Reset a
  | ClearNotes a

ui :: forall eff. String -> H.Component State Query (Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff)))
ui name = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.tr_ $
      [ HH.td_ [ HH.text name ] ] <>
      map
        (\i -> HH.td
            ([ HP.classes $
                [ HH.className $ case state.notes ^? ix i of
                    Just true -> "on"
                    _ -> "off" ]
                <> if (state.phase - 1) `mod` (numNotes state) == i then [ HH.className "playing" ] else []
             ] <> onMouseDownOrTouchStart (ToggleNote i))
            [])
        (range 0 (numNotes state - 1))

  eval :: Query ~> H.ComponentDSL State Query (Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff)))
  eval (LoadSample location next) = do
    sample <- fromAff $ do
      sampleData <- get location
      loadSample sampleData.response
    H.modify \state -> state { sample = Just sample }
    pure next
  eval (Tick next) = do
    state <- H.get
    fromEff $ case state.sample of
      Just sample ->
        when (isNoteOn state (state.phase `mod` (numNotes state))) (play sample)
      _ -> pure unit
    H.modify \s -> s { phase = s.phase + 1 }
    pure next
  eval (ToggleNote note next) = do
    H.modify \state -> state { notes = ix note %~ not $ state.notes }
    pure next
  eval (Reset next) = do
    H.modify \state -> state { phase = 0 }
    pure next
  eval (ClearNotes next) = do
    H.modify \state -> state { notes = replicate (length state.notes) false }
    pure next
