module Main where

import Prelude
import RequestAnimationFrame
import HalogenUtil
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Forms as HF
import Halogen.HTML.Properties as HP
import Instrument as I
import Audio (AUDIO, loadSample, play, Sample)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Free (fromAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (replicate)
import Data.Foldable (sequence_)
import Data.Functor.Coproduct (Coproduct, left)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.Util (awaitBody, runHalogenAff)
import Network.HTTP.Affjax (AJAX, get)


data PlayState = Stopped | Playing Int | Paused Int

type State = { tempo :: Int, sample :: Maybe Sample, playState :: PlayState }

initialState :: forall eff. State' (App eff)
initialState = H.parentState { tempo: 120, sample: Nothing, playState: Playing 0 }

newtype InstrumentSlot = InstrumentSlot String
derive instance eqInstrumentSlot :: Eq InstrumentSlot
derive instance ordInstrumentSlot :: Ord InstrumentSlot


data Query a
  = Init a
  | UpdateTempo String a
  | DecrTempo a
  | IncrTempo a
  | Tick a
  | AskTempo (Number -> a)
  | EnableIosAudio a
  | Stop a
  | Play a
  | Pause a
  | ClearNotes a

type App eff = Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff))

type State' g = H.ParentState State I.State Query I.Query g InstrumentSlot
type Query' = Coproduct Query (H.ChildF InstrumentSlot I.Query)

instruments :: Array String
instruments = ["sounds/kick.wav", "sounds/snare.wav", "sounds/metronome.wav"]

controlButton :: forall a. String -> H.Action Query -> H.HTML a Query
controlButton iconName act =
  HH.i [ HE.onClick (HE.input_ act)
       , HP.classes
            [ HH.className "control"
            , HH.className "fa"
            , HH.className ("fa-" <> iconName)
            , HH.className "fa-3x" ]
            ]
       []

ui :: forall eff. H.Component (State' (App eff)) Query' (App eff)
ui = H.parentComponent { render, eval, peek }
  where

  render :: State -> H.ParentHTML I.State Query I.Query (App eff) InstrumentSlot
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
        , HH.table_ $ map
            (\name ->
              HH.slot (InstrumentSlot name) \_ ->
                { component: I.ui
                , initialState: { sample: Nothing, notes: replicate 16 false, phase: 0 } })
            instruments
        , HH.p [ HE.onMouseDown (HE.input_ EnableIosAudio) ] [ HH.text "Enable audio (iOs)" ]
        ]
      ]

  eval :: Query ~> H.ParentDSL State I.State Query I.Query (App eff) InstrumentSlot
  eval (Init next) = do
    sample <- fromAff $ do
      sampleData <- get "sounds/bd01.wav"
      loadSample sampleData.response
    H.modify \state -> state { sample = Just sample }
    sequence_ $ map (\name -> H.query (InstrumentSlot name) (H.action (I.LoadSample name))) instruments
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
    case state.playState of
      Stopped -> pure unit
      Paused _ -> pure unit
      Playing phase -> do
        sequence_ $ map (\name -> H.query (InstrumentSlot name) (H.action I.Tick)) instruments
        H.modify (\s -> s { playState = Playing (phase + 1) })
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
    sequence_ $ map (\name -> H.query (InstrumentSlot name) (H.action I.Reset)) instruments
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
    sequence_ $ map (\name -> H.query (InstrumentSlot name) (H.action I.ClearNotes)) instruments
    pure next

  peek = Nothing

mainLoop :: forall e. H.Driver Query' (console :: CONSOLE | e) -> Aff (H.HalogenEffects (console :: CONSOLE | e)) Unit
mainLoop driver = loop 0.0
  where
    loop lastTick = do
      tempo <- driver (H.request (left <<< AskTempo))
      requestAnimationFrame \time ->
        let delta = time - lastTick
        in  if delta > tempo
              then do
                driver (H.action (left <<< Tick))
                loop (time - delta `mod` tempo)
              else loop lastTick

main :: Eff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO)) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI ui initialState body
  driver (H.action (left <<< Init))
  mainLoop driver
