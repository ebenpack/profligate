module Profligate.Component where

import Prelude

import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
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
import Text.Parsing.StringParser (runParser, ParseError(..))
import Web.Event.Event (EventType(..), preventDefault, stopPropagation)
import Web.File.Blob (Blob)
import Web.File.File (toBlob)
import Web.File.FileList (item)
import Web.File.FileReader (FileReader, result, fileReader, readAsText, toEventTarget)
import Web.HTML.Event.DataTransfer (DataTransfer, files)
import Web.HTML.Event.DragEvent (DragEvent, dataTransfer, toEvent)

data DisplayMode = 
    FlameGraph
  | TreeViz

derive instance eqDisplayMode :: Eq DisplayMode

data Query =
      UploadFile DragEvent
    | FileLoaded FileReader
    | DragOver DragEvent
    | ChangeDisplayMode DisplayMode
    | NoOp

type State =
    { profFile :: Maybe Profile
    , parseError :: Maybe String
    , displayMode :: DisplayMode
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
        , displayMode: TreeViz
        , loading: false
        }

    header :: H.ComponentHTML Query ChildSlots m
    header =
        HH.header
            [ HE.onDragOver (\e -> Just (DragOver e))
            , HE.onDrop (\e -> Just (UploadFile e))
            ]
            [ HH.h1_ [ HH.text "Profligate" ]
            , HH.div
                [ HP.attr (H.AttrName "class") "dropzone" ]
                [ HH.p_
                    [ HH.text "Plop yer .prof file here" ]
                ]
            ]

    render :: State -> H.ComponentHTML Query ChildSlots m
    render state =
        HH.div
            [ HP.attr (H.AttrName "class") "container" ]
            [ header
            , HH.main_ $ showMain state
            ]

    showMain :: State -> Array (H.ComponentHTML Query ChildSlots m)
    showMain { loading } | loading = [ spinner ]
    showMain { parseError: Just _ } = showError
    showMain { profFile: Just prof, displayMode }
        | displayMode == FlameGraph = [ HH.slot _a unit (FG.flameGraph prof) unit absurd ]
        | displayMode == TreeViz = [ HH.slot _b unit (TV.treeViz prof) unit absurd ]
    showMain _ =
        [ HH.div
            [ HP.attr (H.AttrName "class") "text" ]
            [ HH.h2_ [ HH.text "Introduction" ]
            , HH.p_
                [ HH.text "Profligate is a tool to help you visualize your GHC profile files like a pro... file." ]
            , HH.p_
                [ HH.text $ "Just drop your .prof file (generated with the GHC RTS -p flag) up above, and " <>
                    "profligate will produce a flame graph-style visualization of your program's cost-centre stacks to help you " <>
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
        ChangeDisplayMode mode -> do
            _ <- H.modify $ \state -> state { displayMode = mode }
            pure unit
        FileLoaded fr -> do
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
                    subthingy et fr
                    t <- H.liftEffect $ readAsText blob' fr
                    pure unit
            pure unit
        where
        subthingy trgt fr =
            void $ H.subscribe $ 
                HES.eventListenerEventSource (EventType "load") trgt \evt -> pure $ FileLoaded fr

getFile :: DataTransfer -> Maybe Blob
getFile dt = do
    fl <- files dt
    f <- item 0 fl
    pure $ toBlob f
