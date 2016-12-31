module Main where

import Prelude
import RequestAnimationFrame
import HalogenUtil
import Data.Lens
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Forms as HF
import Halogen.HTML.Properties as HP
import Audio (AUDIO, loadSample, play, Sample)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Free (fromAff, fromEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (concatMap, cons, head, length, range, replicate, take, uncons, (!!))
import Data.Foldable (sequence_)
import Data.Int (fromString, toNumber)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Halogen.Util (awaitBody, runHalogenAff)
import Network.HTTP.Affjax (AJAX, get)


data PlayState = Stopped | Playing Int | Paused Int

isNoteOn :: Array (Array Boolean) -> Int -> Int -> Boolean
isNoteOn notes instrument note =
  case notes ^? ix instrument <<< ix note of
    Just true -> true
    _ -> false

type State =
  { beats :: Int
  , tempo :: Int
  , sample :: Maybe Sample
  , playState :: PlayState
  , instruments :: Array Instrument
  , notes :: Array (Array Boolean)
  , modalOpen :: Boolean
  }

initialState :: State
initialState =
  { beats: 16
  , tempo: 120
  , sample: Nothing
  , playState: Playing 0
  , instruments: []
  , notes: []
  , modalOpen: false
  }

data Query a
  = Init a
  | UpdateTempo String a
  | DecrTempo a
  | IncrTempo a
  | UpdateBeats String a
  | DecrBeats a
  | IncrBeats a
  | Tick a
  | AskTempo (Number -> a)
  | EnableIosAudio a
  | Stop a
  | Play a
  | Pause a
  | ClearNotes a
  | ToggleNote Int Int a
  | PlaySample String a
  | AddInstrument InstrumentSpec a
  | OpenModal a
  | CloseModal a

type App eff = Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff))

type Instrument = { name :: String, sample :: Sample }

type InstrumentSpec = { name :: String, file :: String }

type InstrumentSetSpec = { name :: String, instruments :: Array InstrumentSpec }

instrumentSets :: Array InstrumentSetSpec
instrumentSets =
  [ { name: "Acoustic"
    , instruments:
        [ { name: "Kick", file: "sounds/acoustic/kick.wav" }
        , { name: "Snare", file: "sounds/acoustic/snare.wav" }
        , { name: "Hi-hat", file: "sounds/acoustic/hihat.wav" }
        , { name: "Metronome", file: "sounds/acoustic/metronome.wav" }
        ]
    }
  , { name: "C64"
    , instruments:
        [ { name: "Clap", file: "sounds/c64/clap.wav" }
        , { name: "Cowbell", file: "sounds/c64/cowbell.wav" }
        , { name: "Hihat 1", file: "sounds/c64/hihat_1.wav" }
        , { name: "Hihat 2", file: "sounds/c64/hihat_2.wav" }
        , { name: "Kick 1", file: "sounds/c64/kick_1.wav" }
        , { name: "Kick 2", file: "sounds/c64/kick_2.wav" }
        , { name: "Kick 3", file: "sounds/c64/kick_3.wav" }
        , { name: "Kick 4", file: "sounds/c64/kick_4.wav" }
        , { name: "Kick 5", file: "sounds/c64/kick_5.wav" }
        , { name: "Kick 6", file: "sounds/c64/kick_6.wav" }
        , { name: "Kick 7", file: "sounds/c64/kick_7.wav" }
        , { name: "Kick 8", file: "sounds/c64/kick_8.wav" }
        , { name: "Snare 1", file: "sounds/c64/snare_1.wav" }
        , { name: "Snare 2", file: "sounds/c64/snare_2.wav" }
        , { name: "Snare 3", file: "sounds/c64/snare_3.wav" }
        , { name: "Snare 4", file: "sounds/c64/snare_4.wav" }
        , { name: "Snare 5", file: "sounds/c64/snare_5.wav" }
        , { name: "Snare 6", file: "sounds/c64/snare_6.wav" }
        , { name: "Snare 7", file: "sounds/c64/snare_7.wav" }
        , { name: "Snare 8", file: "sounds/c64/snare_8.wav" }
        , { name: "Tom 1", file: "sounds/c64/tom_1.wav" }
        , { name: "Tom 2", file: "sounds/c64/tom_2.wav" }
        ]
    }
  ]

loadInstrument :: forall eff. InstrumentSpec -> Aff (ajax :: AJAX, audio :: AUDIO | eff) Instrument
loadInstrument instrument = do
  sampleData <- get instrument.file
  sample <- loadSample sampleData.response
  pure { name: instrument.name, sample: sample }

loadSet :: forall eff. InstrumentSetSpec -> Aff (ajax :: AJAX, audio :: AUDIO | eff) (Array Instrument)
loadSet spec = traverse loadInstrument spec.instruments

controlButton :: forall a. String -> H.Action Query -> H.HTML a Query
controlButton iconName act =
  HH.i (onMouseDownOrTouchStart act <>
          [ HP.classes
            [ HH.className "control"
            , HH.className "fa"
            , HH.className ("fa-" <> iconName)
            , HH.className "fa-3x"
            ]
          ])
       []

zipWithIndex :: forall a b. (a -> Int -> b) -> Array a -> Array b
zipWithIndex f = go 0
  where
    go i arr = case uncons arr of
      Just { head: x, tail: xs } -> cons (f x i) (go (i+1) xs)
      Nothing -> []

updateBeats :: Int -> State -> State
updateBeats beats state =
  state { beats = beats
        , notes = map (\row -> take beats row <> replicate (beats - state.beats) false) state.notes
        }

ui :: forall eff. H.Component State Query (App eff)
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (HH.className "main") ] $
      [ HH.div_
        [ HH.div [ HP.class_ (HH.className "controls") ]
            [ HH.div [ HP.class_ (HH.className "playback") ]
                [ controlButton "stop" Stop
                , case state.playState of
                    Playing _ -> controlButton "pause" Pause
                    _ -> controlButton "play" Play
                ]
            , HH.div [ HP.class_ (HH.className "tempo") ]
                [ HH.button (onMouseDownOrTouchStart DecrTempo) [ HH.text "−10" ]
                , HH.input [ HF.onValueInput (HE.input UpdateTempo), HP.placeholder (show state.tempo) ]
                , HH.button (onMouseDownOrTouchStart IncrTempo) [ HH.text "+10" ]
                ]
            , HH.div [ HP.class_ (HH.className "tempo") ]
                [ HH.button (onMouseDownOrTouchStart DecrBeats) [ HH.text "−" ]
                , HH.input [ HF.onValueInput (HE.input UpdateBeats), HP.placeholder (show state.beats) ]
                , HH.button (onMouseDownOrTouchStart IncrBeats) [ HH.text "+" ]
                ]
            , controlButton "trash-o" ClearNotes
            ]
        , HH.div [ HP.class_ (HH.className "controls") ]
            [ HH.table_ $ map (renderInstrument state) (range 0 (length state.notes - 1))
            ]
        , HH.div [ HP.class_ (HH.className "controls") ]
            [ controlButton "plus" OpenModal ]
        , HH.p [ HE.onMouseDown (HE.input_ EnableIosAudio) ] [ HH.text "Enable audio (iOs)" ]
        ]
      ] <>
      if state.modalOpen
        then
          [ HH.div [ HP.class_ (HH.className "modal") ]
              [ HH.div [ HP.class_ (HH.className "modal-body") ]
                [ HH.table_ $ flip concatMap instrumentSets (\is -> flip map is.instruments \i ->
                    HH.tr_
                      [ HH.td_ [ HH.text is.name ]
                      , HH.td_ [ HH.text i.name ]
                      , HH.td_ [ HH.i (onMouseDownOrTouchStart (PlaySample i.file) <> [ HP.classes [ HH.className "fa", HH.className "fa-2x", HH.className "fa-volume-up" ] ]) [] ]
                      , HH.td_ [ HH.i (onMouseDownOrTouchStart (AddInstrument i) <> [ HP.classes [ HH.className "fa", HH.className "fa-2x", HH.className "fa-plus" ] ]) [] ]
                      ]
                  )
                ]
              ]
          , HH.div ([ HP.class_ (HH.className "modal-background") ] <> onMouseDownOrTouchStart CloseModal) []
          ]
        else []

  eval :: Query ~> H.ComponentDSL State Query (App eff)
  eval (Init next) = do
    sample <- fromAff $ do
      sampleData <- get "sounds/bd01.wav"
      loadSample sampleData.response
    instruments <-
      case head instrumentSets of
        Just set -> fromAff $ loadSet set
        Nothing -> pure []
    H.modify \state -> state { sample = Just sample
                             , instruments = instruments
                             , notes = replicate (length instruments) (replicate 16 false)
                             }
    pure next
  eval (UpdateTempo tempo next) = do
    H.modify (\state -> state { tempo = fromMaybe state.tempo (fromString tempo) })
    pure next
  eval (DecrTempo next) = do
    H.modify (\state -> state { tempo = state.tempo - 10 })
    pure next
  eval (IncrTempo next) = do
    H.modify (\state -> state { tempo = state.tempo + 10 })
    pure next
  eval (UpdateBeats beatsStr next) = do
    state <- H.get
    H.modify $ updateBeats (fromMaybe state.beats (fromString beatsStr))
    pure next
  eval (DecrBeats next) = do
    state <- H.get
    H.modify $ updateBeats (state.beats - 1)
    pure next
  eval (IncrBeats next) = do
    state <- H.get
    H.modify $ updateBeats (state.beats + 1)
    pure next
  eval (Tick next) = do
    state <- H.get
    case state.playState of
      Stopped -> pure unit
      Paused _ -> pure unit
      Playing phase -> do
        H.modify (\s -> s { playState = Playing ((phase + 1) `mod` state.beats) })
        sequence_ $ flip zipWithIndex state.instruments \instrument idx ->
          when (isNoteOn state.notes idx phase) (fromEff $ play instrument.sample)
    pure next
  eval (AskTempo k) = do
    state <- H.get
    pure (k (60000.0 / toNumber state.tempo / 4.0))
  eval (EnableIosAudio next) = do
    state <- H.get
    case state.sample of
      Just sample -> fromAff $ liftEff $ play sample
      Nothing -> pure unit
    pure next
  eval (Stop next) = do
    H.modify \state -> state { playState = Stopped }
    pure next
  eval (Play next) = do
    H.modify \state ->
      case state.playState of
        Stopped -> state { playState = Playing 0 }
        Paused phase -> state { playState = Playing phase }
        Playing _ -> state
    pure next
  eval (Pause next) = do
    H.modify \state ->
      case state.playState of
        Playing phase -> state { playState = Paused phase }
        _ -> state
    pure next
  eval (ClearNotes next) = do
    H.modify \state -> state { notes = map (map (const false)) state.notes }
    pure next
  eval (ToggleNote instrument note next) = do
    fromAff $ log "foo"
    H.modify \state -> state { notes = ix instrument <<< ix note %~ not $ state.notes }
    pure next
  eval (PlaySample location next) = do
    sample <- fromAff $ do
      sampleData <- get location
      loadSample sampleData.response
    fromEff $ play sample
    pure next
  eval (AddInstrument spec next) = do
    instrument <- fromAff $ loadInstrument spec
    H.modify \state -> state { instruments = state.instruments <> [ instrument ]
                             , notes = state.notes <> [ replicate state.beats false ]
                             }
    pure next
  eval (OpenModal next) = do
    H.modify \state -> state { modalOpen = true }
    pure next
  eval (CloseModal next) = do
    H.modify \state -> state { modalOpen = false }
    pure next

renderInstrument :: State -> Int -> H.ComponentHTML Query
renderInstrument state instrument =
  HH.tr_ $
    case state.instruments !! instrument of
      Just instr -> [ HH.td_ [ HH.text instr.name ] ]
      Nothing -> []
    <> map
      (\i -> HH.td
          ([ HP.classes $
              [ HH.className $ if isNoteOn state.notes instrument i then "on" else "off" ]
              <> case state.playState of
                    Playing phase ->
                      if phase == i
                        then [ HH.className "playing" ]
                        else []
                    Paused phase ->
                      if phase == i
                        then [ HH.className "paused" ]
                        else []
                    Stopped -> []
           ] <> onMouseDownOrTouchStart (ToggleNote instrument i))
          [])
      (range 0 (state.beats - 1))

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

main :: Eff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO)) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI ui initialState body
  driver (H.action Init)
  mainLoop driver
