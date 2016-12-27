module Main where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Forms as HF
import Halogen.HTML.Properties as HP
import Audio (initContext, play)
import Control.Monad.Aff (Aff, later')
import Control.Monad.Aff.Free (fromAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM.File.Types (Blob)
import Data.Array (range)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio (Ratio(..), gcd)
import Halogen (action)
import Halogen.HTML.Core (className)
import Halogen.Util (awaitBody, runHalogenAff)
import Network.HTTP.Affjax (get, AJAX)


lcm :: Int -> Int -> Int
lcm a b = (a * b) / gcd (Ratio a b)

type SoundContext =
  { ctx :: Blob
  , metronome :: ArrayBuffer
  , kick :: ArrayBuffer
  , snare :: ArrayBuffer
  }

type State = { a :: Int, b :: Int, soundCtx :: Maybe SoundContext, phase :: Int }

initialState :: State
initialState = { a: 3, b: 4, soundCtx: Nothing, phase: 0 }

data Query a
  = Init a
  | UpdateA String a
  | UpdateB String a
  | Tick a

ui :: forall eff. H.Component State Query (Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX | eff)))
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (className "main") ]
      [ HH.div_
        [ HH.table_
            [ renderRepeat state.a (lcm state.a state.b)
            , renderRepeat state.b (lcm state.a state.b)
            ]
        , HH.div [ HP.class_ (className "inputs") ]
            [ HH.input [ HF.onValueInput (HE.input UpdateA), HP.placeholder (show state.a) ]
            , HH.input [ HF.onValueInput (HE.input UpdateB), HP.placeholder (show state.b) ]
            ]
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX | eff)))
  eval (Init next) = do
    metronome <- fromAff $ get "sounds/metronome.wav"
    kick <- fromAff $ get "sounds/kick.wav"
    snare <- fromAff $ get "sounds/snare.wav"
    H.modify (\state -> state { soundCtx = Just { ctx: initContext 5, metronome: metronome.response, kick: kick.response, snare: snare.response } })
    pure next
  eval (UpdateA aStr next) = do
    H.modify (\state -> state { a = fromMaybe state.a (fromString aStr) })
    pure next
  eval (UpdateB bStr next) = do
    H.modify (\state -> state { b = fromMaybe state.b (fromString bStr) })
    pure next
  eval (Tick next) = do
    state <- H.get
    case state.soundCtx of
      Just soundCtx -> do
        if state.phase `mod` state.a == 0
          then pure (play soundCtx.ctx soundCtx.kick)
          else pure unit
        if state.phase `mod` state.b == 0
          then pure (play soundCtx.ctx soundCtx.snare)
          else pure unit
      Nothing -> pure unit
    H.modify (\s -> s { phase = s.phase + 1 })
    pure next

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a

renderRepeat :: forall a. Int -> Int -> H.ComponentHTML a
renderRepeat cycle total = HH.tr_ $
  map
    (\i -> HH.td [ HP.class_ (className (if i `mod` cycle == 0 then "on" else "off")) ] [])
    (range 0 (total - 1))

main :: forall eff. Eff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX | eff)) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI ui initialState body
  driver (action Init)
  setInterval 200 (driver (action Tick))
