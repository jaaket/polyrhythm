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
import Data.Array (cons, length, range, replicate, take, uncons, (!!))
import Data.Foldable (sequence_, traverse_)
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
  , instrumentSamples :: Maybe (Array Sample)
  , notes :: Array (Array Boolean)
  }

initialState :: State
initialState =
  { beats: 16
  , tempo: 120
  , sample: Nothing
  , playState: Playing 0
  , instrumentSamples: Nothing
  , notes: []
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

type App eff = Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff))

instrumentSpecs :: Array { name :: String, file :: String }
instrumentSpecs =
  [ { name: "Kick", file: "sounds/kick.wav" }
  , { name: "Snare", file: "sounds/snare.wav" }
  , { name: "Metronome", file: "sounds/metronome.wav" }
  ]

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
    HH.div [ HP.class_ (HH.className "main") ]
      [ HH.div_
        [ HH.div [ HP.class_ (HH.className "controls") ]
            [ HH.div [ HP.class_ (HH.className "playback") ]
                [ controlButton "stop" Stop
                , case state.playState of
                    Playing _ -> controlButton "pause" Pause
                    _ -> controlButton "play" Play
                ]
            , HH.div [ HP.class_ (HH.className "tempo") ]
                [ HH.button [ HE.onClick (HE.input_ DecrTempo) ] [ HH.text "−10" ]
                , HH.input [ HF.onValueInput (HE.input UpdateTempo), HP.placeholder (show state.tempo) ]
                , HH.button [ HE.onClick (HE.input_ IncrTempo) ] [ HH.text "+10" ]
                ]
            , controlButton "trash-o" ClearNotes
            ]
        , HH.div [ HP.class_ (HH.className "controls") ]
            [ HH.table_ $ map (renderInstrument state) (range 0 (length state.notes - 1))
            ]
        , HH.div [ HP.class_ (HH.className "tempo") ]
            [ HH.button [ HE.onClick (HE.input_ DecrBeats) ] [ HH.text "−" ]
            , HH.input [ HF.onValueInput (HE.input UpdateBeats), HP.placeholder (show state.beats) ]
            , HH.button [ HE.onClick (HE.input_ IncrBeats) ] [ HH.text "+" ]
            ]
        , HH.p [ HE.onMouseDown (HE.input_ EnableIosAudio) ] [ HH.text "Enable audio (iOs)" ]
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query (App eff)
  eval (Init next) = do
    sample <- fromAff $ do
      sampleData <- get "sounds/bd01.wav"
      loadSample sampleData.response
    instrumentSamples <- flip traverse instrumentSpecs \instrument ->
      fromAff $ do
        sampleData <- get instrument.file
        loadSample sampleData.response
    H.modify \state -> state { sample = Just sample
                             , instrumentSamples = Just instrumentSamples
                             , notes = replicate (length instrumentSpecs) (replicate 16 false)
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
        case state.instrumentSamples of
          Just samples ->
            sequence_ $ flip zipWithIndex state.notes \instrument idx ->
              case samples !! idx of
                Just sample ->
                  when (isNoteOn state.notes idx phase) (fromEff $ play sample)
                Nothing -> pure unit
          _ -> pure unit

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

renderInstrument :: State -> Int -> H.ComponentHTML Query
renderInstrument state instrument =
  HH.tr_ $
    -- [ HH.td_ [ HH.text name ] ] <>
    map
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
