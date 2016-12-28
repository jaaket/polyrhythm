module Main where

import Prelude
import RequestAnimationFrame
import Data.Lens
import Data.Lens.Index (ix)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Forms as HF
import Halogen.HTML.Properties as HP
import Audio (AUDIO, Sample, loadSample, play)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Free (fromAff, fromEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Parallel (parallel, sequential)
import Data.Array (range)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio (Ratio(..), gcd)
import Halogen.Util (awaitBody, runHalogenAff)
import Network.HTTP.Affjax (get, AJAX)


lcm :: Int -> Int -> Int
lcm a b = (a * b) / gcd (Ratio a b)

type Sounds =
  { metronome :: Sample
  , kick :: Sample
  , snare :: Sample
  }

type State = { a :: Int, b :: Int, notes :: Array (Array Boolean), numNotes :: Int, sounds :: Maybe Sounds, phase :: Int, tempo :: Int }

initialState :: State
initialState = { a: 3, b: 4, notes: [], numNotes: 0, sounds: Nothing, phase: 0, tempo: 120 }

isNoteOn :: State -> Int -> Int -> Boolean
isNoteOn state row column =
  case state.notes ^? ix row <<< ix column of
    Just true -> true
    _ -> false

generatePolyrhythm :: Int -> Int -> Array (Array Boolean)
generatePolyrhythm a b =
  [ map (\i -> i `mod` (lcm a b / a) == 0) (range 0 (lcm a b - 1))
  , map (\i -> i `mod` (lcm a b / b) == 0) (range 0 (lcm a b - 1))
  ]

toggleNote :: Int -> Int -> Array (Array Boolean) -> Array (Array Boolean)
toggleNote row column = ix row <<< ix column %~ not

data Query a
  = Init a
  | UpdateA String a
  | UpdateB String a
  | UpdateTempo String a
  | DecrTempo a
  | IncrTempo a
  | Tick a
  | AskTempo (Number -> a)
  | ClickCell Int Int a

ui :: forall eff. H.Component State Query (Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff)))
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.className "main") ]
      [ HH.div_
        [ HH.div [ HP.class_ (HH.className "tempo") ]
            [ HH.button [ HE.onClick (HE.input_ DecrTempo) ] [ HH.text "−10" ]
            , HH.input [ HF.onValueInput (HE.input UpdateTempo), HP.placeholder (show state.tempo) ]
            , HH.button [ HE.onClick (HE.input_ IncrTempo) ] [ HH.text "+10" ]
            ]
        , HH.table_
            [ renderNotes 0 state
            , renderNotes 1 state
            ]
        , HH.div [ HP.class_ (HH.className "inputs") ]
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
        <*> parallel (get "sounds/bd01.wav")
        <*> parallel (get "sounds/sd03.wav")
    metronome <- fromAff (loadSample samples.metronome)
    kick <- fromAff (loadSample samples.kick)
    snare <- fromAff (loadSample samples.snare)
    H.modify (\state -> state
      { sounds = Just { metronome: metronome, kick: kick, snare: snare }
      , notes = generatePolyrhythm 3 4
      , numNotes = lcm 3 4
      })
    pure next
  eval (UpdateA aStr next) = do
    H.modify (\state -> state { a = fromMaybe state.a (fromString aStr) })
    H.modify (\state -> state { notes = generatePolyrhythm state.a state.b, numNotes = lcm state.a state.b })
    pure next
  eval (UpdateB bStr next) = do
    H.modify (\state -> state { b = fromMaybe state.b (fromString bStr) })
    H.modify (\state -> state { notes = generatePolyrhythm state.a state.b, numNotes = lcm state.a state.b })
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
        when (isNoteOn state 0 (state.phase `mod` state.numNotes)) (play sounds.kick)
        when (isNoteOn state 1 (state.phase `mod` state.numNotes)) (play sounds.snare)
      Nothing -> pure unit
    H.modify (\s -> s { phase = s.phase + 1 })
    pure next
  eval (AskTempo k) = do
    state <- H.get
    pure (k (60000.0 / toNumber state.tempo / (toNumber (lcm state.a state.b) / toNumber state.a)))
  eval (ClickCell row column next) = do
    H.modify \state -> state { notes = toggleNote row column state.notes }
    pure next

mainLoop :: forall e. H.Driver Query (console :: CONSOLE | e) -> Aff (H.HalogenEffects (console :: CONSOLE | e)) Unit
mainLoop driver = loop 0.0
  where
    loop lastTick = do
      tempo <- driver (H.request AskTempo)
      requestAnimationFrame \time ->
        let delta = time - lastTick
        in  if delta > tempo
              then do
                driver (H.action Tick)
                loop (time - delta `mod` tempo)
              else loop lastTick

renderNotes :: Int -> State -> H.ComponentHTML Query
renderNotes row state = HH.tr_ $
  map
    (\i -> HH.td
        [ HP.classes $
            [ HH.className $ case state.notes ^? ix row <<< ix i of
                Just true -> "on"
                _ -> "off" ]
            <> if (state.phase - 1) `mod` state.numNotes == i then [ HH.className "playing" ] else []
        , HE.onClick (HE.input_ (ClickCell row i))
        ] [])
    (range 0 (state.numNotes - 1))

main :: Eff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO)) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI ui initialState body
  driver (H.action Init)
  mainLoop driver
