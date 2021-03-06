module Profligate.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (delay)
import Effect.Class (class MonadEffect)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HES
import Profligate.FlameGraph as FG
import Profligate.Profile.ParseProfile (parseProfFile)
import Profligate.Profile.Profile (Profile, filter)
import Profligate.Spinner (spinner)
import Profligate.TreeViz as TV
import Profligate.Types (AnalysisMode(..), DisplayMode(..))
import Text.Parsing.StringParser (runParser, ParseError(..))
import Web.Event.Event (EventType(..), preventDefault, stopPropagation)
import Web.File.Blob (Blob)
import Web.File.File (toBlob)
import Web.File.FileList (item)
import Web.File.FileReader (FileReader, result, fileReader, readAsText, toEventTarget)
import Web.HTML.Event.DataTransfer (DataTransfer, files)
import Web.HTML.Event.DragEvent (DragEvent, dataTransfer, toEvent)

data Query =
      UploadFile DragEvent
    | FileLoaded FileReader
    | DragOver DragEvent
    | SetDisplayMode DisplayMode
    | SetAnalysisMode AnalysisMode
    | NoOp

type State =
    { profFile :: Maybe Profile
    , parseError :: Maybe String
    , displayMode :: DisplayMode
    , analysisMode :: AnalysisMode
    , loading :: Boolean
    }

type ChildSlots =
  ( a :: FG.Slot Unit
  , b :: TV.Slot Unit
  )

_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"

component :: forall f i o m. MonadEffect m => MonadAff m => H.Component HH.HTML f i o m
component =
    H.mkComponent
        { initialState: const initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = eval }
        }
    where
    initialState :: State
    initialState =
        { profFile: Nothing
        , parseError: Nothing
        , displayMode: FlameGraph
        , analysisMode: Time
        , loading: false
        }

    header :: State -> H.ComponentHTML Query ChildSlots m
    header state =
        HH.header
            [ HE.onDragOver (\e -> Just (DragOver e))
            , HE.onDrop (\e -> Just (UploadFile e))
            ]
            [ HH.h1_ [ HH.text "Profligate" ]
            , HH.div
                [ HP.attr (H.AttrName "class") "displayToggle" ]
                [ HH.p_
                    [ HH.span_
                        [ HH.label
                            [ HP.attr (H.AttrName "class") "switch"  ]
                            [ HH.input
                                ([ HE.onClick (\e -> Just $ SetDisplayMode FlameGraph)
                                , HP.attr (H.AttrName "type") "radio"
                                , HP.attr (H.AttrName "name") "displayMode"
                                ]  <> if state.displayMode == FlameGraph then [ HP.attr (H.AttrName "checked") "checked" ] else [])
                            , HH.span
                                []
                                [ HH.text "Flame Graph" ]
                            ]
                        ]
                    , HH.span_
                        [ HH.label
                            [ HP.attr (H.AttrName "class") "switch"  ]
                            [ HH.input
                                ([ HE.onClick (\e -> Just $ SetDisplayMode TreeViz)
                                , HP.attr (H.AttrName "type") "radio"
                                , HP.attr (H.AttrName "name") "displayMode"
                                ]  <> if state.displayMode == TreeViz then [ HP.attr (H.AttrName "checked") "checked" ] else [])
                            , HH.span
                                []
                                [ HH.text "Tree Vizualizer" ]
                            ]
                        ]
                    ]
                , HH.p_
                    [ HH.span_
                        [ HH.label
                            [ HP.attr (H.AttrName "class") "switch"  ]
                            [ HH.input
                                ([ HE.onClick (\e -> Just $ SetAnalysisMode Time)
                                , HP.attr (H.AttrName "type") "radio"
                                , HP.attr (H.AttrName "name") "analysisMode"
                                ]  <> if state.analysisMode == Time then [ HP.attr (H.AttrName "checked") "checked" ] else [])
                            , HH.span
                                []
                                [ HH.text "Time" ]
                            ]
                        ]
                    , HH.span_
                        [ HH.label
                            [ HP.attr (H.AttrName "class") "switch"  ]
                            [ HH.input
                                ([ HE.onClick (\e -> Just $ SetAnalysisMode Alloc)
                                , HP.attr (H.AttrName "type") "radio"
                                , HP.attr (H.AttrName "name") "analysisMode"
                                ]  <> if state.analysisMode == Alloc then [ HP.attr (H.AttrName "checked") "checked" ] else [])
                            , HH.span
                                []
                                [ HH.text "Alloc" ]
                            ]
                        ]
                    ]
                ]
            , HH.div
                [ HP.attr (H.AttrName "class") "dropzone" ]
                [ HH.p_  [ HH.text "Plop yer .prof file here" ] ]
            ]

    render :: State -> H.ComponentHTML Query ChildSlots m
    render state =
        HH.div
            [ HP.attr (H.AttrName "class") "container" ]
            [ header state
            , HH.main_ $ showMain state
            ]

    showMain :: State -> Array (H.ComponentHTML Query ChildSlots m)
    showMain { loading } | loading = [ spinner ]
    showMain { parseError: Just _ } = showError
    showMain { profFile: Just prof, displayMode, analysisMode }
        | displayMode == FlameGraph = [ HH.slot _a unit (FG.flameGraph prof analysisMode) analysisMode absurd ]
        | displayMode == TreeViz = [ HH.slot _b unit (TV.treeViz prof analysisMode) analysisMode absurd ]
    showMain _ =
        [ HH.div
            [ HP.attr (H.AttrName "class") "text" ]
            [ HH.h2_ [ HH.text "Introduction" ]
            , HH.p_
                [ HH.text "Profligate is a tool to help you visualize your GHC profile files like a pro... file." ]
            , HH.p_
                [ HH.text $ "Just drop your .prof file (generated with the GHC RTS -p flag) up above, and " <>
                    "profligate will produce a visualization of your program's cost-centres to help you " <>
                    "analyze your program's performance."
                ]
            , HH.p_
                [ HH.text "If you just want to try it out, "
                , HH.a
                    [ HP.attr (H.AttrName "href") "hascheme.prof"]
                    [ HH.text "there's a sample .prof file you can use here." ]
                ]
            ]
        ]

    showError :: Array (H.ComponentHTML Query ChildSlots m)
    showError =
        [ HH.div
            [ HP.attr (H.AttrName "class") "text" ]
            [ HH.h2_ [ HH.text "Whoopsie daisy..." ]
            , HH.p_ [ HH.text "A problem occurred while processing your file." ]
            , HH.p_
                [ HH.text "If you believe this .prof file to be valid and well-formed, please "
                , HH.a
                    [ HP.attr (H.AttrName "href") "https://github.com/ebenpack/profligate/issues"]
                    [ HH.text "submit a bug report here" ]
                , HH.text ", attaching this file."
                ]
            ]
        ]

    eval :: Query -> H.HalogenM State Query ChildSlots o m Unit
    eval = case _ of
        NoOp -> do
            pure unit
        SetAnalysisMode mode -> do
            _ <- H.modify $ \state -> state { analysisMode = mode }
            pure unit
        SetDisplayMode mode -> do
            _ <- H.modify $ \state -> state { displayMode = mode }
            pure unit
        FileLoaded fr -> do
            -- This is a bit stinky, but a little delay here relinquishes the
            -- main thread before we do some heavy parsing, which allows the UI to
            -- update to show our spinner
            H.liftAff $ delay $ Milliseconds 17.29
            t <- H.liftEffect $ result fr
            let prof = runParser parseProfFile $ unsafeFromForeign t
            _ <- H.modify (\state ->
                case prof of
                    Left (ParseError err) ->
                        state { parseError = Just err, profFile = Nothing, loading = false }
                    Right profFile ->
                        let filteredCostCenterStack = filter (\v -> v.inherited.time > 0.0 || v.inherited.alloc > 0.0) profFile.costCenterStack
                        in state { profFile = Just (profFile { costCenterStack = filteredCostCenterStack }), parseError = Nothing, loading = false })
            pure unit
        DragOver e -> do
            let evt = (toEvent e)
            H.liftEffect $ preventDefault evt
            H.liftEffect $ stopPropagation evt
            pure unit
        UploadFile e -> do
            let evt = toEvent e
            H.liftEffect $ preventDefault evt
            H.liftEffect $ stopPropagation evt
            _ <- H.modify (\state ->
                state { parseError = Nothing, profFile = Nothing, loading = true })
            let trans = dataTransfer e
                blob = getFile trans
            case blob of
                Nothing -> do
                    _ <- H.modify (\state ->
                        state { loading = false })
                    pure unit
                Just blob' -> do
                    fr <- H.liftEffect $ fileReader
                    let et = toEventTarget fr
                    subscribeToUploadEvent et fr
                    H.liftEffect $ readAsText blob' fr
                    pure unit
            pure unit
        where
        subscribeToUploadEvent trgt fr =
            void $ H.subscribe $
                HES.eventListenerEventSource (EventType "load") trgt \evt -> pure $ FileLoaded fr

getFile :: DataTransfer -> Maybe Blob
getFile dt = do
    fl <- files dt
    f <- item 0 fl
    pure $ toBlob f
