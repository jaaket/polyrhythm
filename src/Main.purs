module Main where

import Prelude
import RequestAnimationFrame
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Forms as HF
import Halogen.HTML.Properties as HP
import Instrument as I
import Audio (AUDIO)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Free (fromAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (replicate, zipWith)
import Data.Foldable (sequence_)
import Data.Functor.Coproduct (Coproduct, left)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.Util (awaitBody, runHalogenAff)
import Network.HTTP.Affjax (AJAX)


type State = { phase :: Int, tempo :: Int }

initialState :: forall eff. State' (App eff)
initialState = H.parentState { phase: 0, tempo: 120 }

newtype InstrumentSlot = InstrumentSlot String
derive instance eqInstrumentSlot :: Eq InstrumentSlot
derive instance ordInstrumentSlot :: Ord InstrumentSlot


data Query a
  = Init a
  -- | UpdateA String a
  -- | UpdateB String a
  | UpdateTempo String a
  | DecrTempo a
  | IncrTempo a
  | Tick a
  | AskTempo (Number -> a)

type App eff = Aff (H.HalogenEffects (console :: CONSOLE, ajax :: AJAX, audio :: AUDIO | eff))

type State' g = H.ParentState State I.State Query I.Query g InstrumentSlot
type Query' = Coproduct Query (H.ChildF InstrumentSlot I.Query)

instruments :: Array String
instruments = ["sounds/kick.wav", "sounds/snare.wav", "sounds/metronome.wav"]

ui :: forall eff. H.Component (State' (App eff)) Query' (App eff)
ui = H.parentComponent { render, eval, peek }
  where

  render :: State -> H.ParentHTML I.State Query I.Query (App eff) InstrumentSlot
  render state =
    HH.div [ HP.class_ (HH.className "main") ]
      [ HH.div_
        [ HH.div [ HP.class_ (HH.className "tempo") ]
            [ HH.button [ HE.onClick (HE.input_ DecrTempo) ] [ HH.text "−10" ]
            , HH.input [ HF.onValueInput (HE.input UpdateTempo), HP.placeholder (show state.tempo) ]
            , HH.button [ HE.onClick (HE.input_ IncrTempo) ] [ HH.text "+10" ]
            ]
        , HH.table_ $ map
            (\name ->
              HH.slot (InstrumentSlot name) \_ ->
                { component: I.ui
                , initialState: { sample: Nothing, notes: replicate 16 false, phase: 0 } })
            instruments
        ]
      ]

  eval :: Query ~> H.ParentDSL State I.State Query I.Query (App eff) InstrumentSlot
  eval (Init next) = do
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
    sequence_ $ map (\name -> H.query (InstrumentSlot name) (H.action I.Tick)) instruments
    fromAff $ log "asd"
    H.modify (\s -> s { phase = s.phase + 1 })
    pure next
  eval (AskTempo k) = do
    state <- H.get
    pure (k (60000.0 / toNumber state.tempo / 4.0))

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
