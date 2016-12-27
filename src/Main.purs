module Main where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Forms as HF
import Halogen.HTML.Properties as HP
import Audio (AUDIO, Sample, loadSample, play)
import Control.Monad.Aff (Aff, later')
import Control.Monad.Aff.Console (log)
import Control.Parallel (parallel, sequential)
import Control.Monad.Aff.Free (fromAff, fromEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (range)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio (Ratio(..), gcd)
import Halogen (Driver, action, request)
import Halogen.HTML.Core (className)
import Halogen.Util (awaitBody, runHalogenAff)
import Network.HTTP.Affjax (get, AJAX)


lcm :: Int -> Int -> Int
lcm a b = (a * b) / gcd (Ratio a b)

type Sounds =
  { metronome :: Sample
  , kick :: Sample
  , snare :: Sample
  }

type State = { a :: Int, b :: Int, sounds :: Maybe Sounds, phase :: Int, tempo :: Int }

initialState :: State
initialState = { a: 3, b: 4, sounds: Nothing, phase: 0, tempo: 120 }

data Query a
  = Init a
  | UpdateA String a
  | UpdateB String a
  | UpdateTempo String a
  | DecrTempo a
  | IncrTempo a
  | Tick a
  | AskTempo (Int -> a)

ui :: forall eff. H.Component State Query (Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff)))
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (className "main") ]
      [ HH.div_
        [ HH.div [ HP.class_ (className "tempo") ]
            [ HH.button [ HE.onClick (HE.input_ DecrTempo) ] [ HH.text "−10" ]
            , HH.input [ HF.onValueInput (HE.input UpdateTempo), HP.placeholder (show state.tempo) ]
            , HH.button [ HE.onClick (HE.input_ IncrTempo) ] [ HH.text "+10" ]
            ]
        , HH.table_
            [ renderRepeat state.phase state.a (lcm state.a state.b)
            , renderRepeat state.phase state.b (lcm state.a state.b)
            ]
        , HH.div [ HP.class_ (className "inputs") ]
            [ HH.input [ HF.onValueInput (HE.input UpdateA), HP.placeholder (show state.a) ]
            , HH.input [ HF.onValueInput (HE.input UpdateB), HP.placeholder (show state.b) ]
            ]
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff)))
  eval (Init next) = do
    samples <- fromAff $ sequential $
      (\m k s -> { metronome: m.response, kick: k.response, snare: s.response })
        <$> parallel (get "sounds/metronome.wav")
        <*> parallel (get "sounds/kick.wav")
        <*> parallel (get "sounds/snare.wav")
    metronome <- fromAff (loadSample samples.metronome)
    kick <- fromAff (loadSample samples.kick)
    snare <- fromAff (loadSample samples.snare)
    H.modify (\state -> state { sounds = Just { metronome: metronome, kick: kick, snare: snare } })
    pure next
  eval (UpdateA aStr next) = do
    H.modify (\state -> state { a = fromMaybe state.a (fromString aStr) })
    pure next
  eval (UpdateB bStr next) = do
    H.modify (\state -> state { b = fromMaybe state.b (fromString bStr) })
    pure next
  eval (UpdateTempo tempo next) = do
    H.modify (\state -> state { tempo = fromMaybe state.tempo (fromString tempo) })
    fromAff $ log tempo
    pure next
  eval (DecrTempo next) = do
    H.modify (\state -> state { tempo = state.tempo - 10 })
    pure next
  eval (IncrTempo next) = do
    H.modify (\state -> state { tempo = state.tempo + 10 })
    pure next
  eval (Tick next) = do
    state <- H.get
    fromEff $ case state.sounds of
      Just sounds -> do
        when (state.phase `mod` state.a == 0) (play sounds.kick)
        when (state.phase `mod` state.b == 0) (play sounds.metronome)
      Nothing -> pure unit
    H.modify (\s -> s { phase = s.phase + 1 })
    pure next
  eval (AskTempo k) = do
    state <- H.get
    pure (k (60000 / state.tempo / state.a))

mainLoop :: forall e. Driver Query e -> Aff (H.HalogenEffects e) Unit
mainLoop driver = loop
  where
    loop = do
      driver (action Tick)
      tempo <- driver (request AskTempo)
      later' tempo loop

renderRepeat :: forall a. Int -> Int -> Int -> H.ComponentHTML a
renderRepeat phase cycle total = HH.tr_ $
  map
    (\i -> HH.td
        [ HP.classes $
            [ className (if i `mod` cycle == 0 then "on" else "off") ]
            <> if (phase - 1) `mod` total == i then [ className "playing" ] else []
        ] [])
    (range 0 (total - 1))

main :: Eff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO)) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI ui initialState body
  driver (action Init)
  mainLoop driver
