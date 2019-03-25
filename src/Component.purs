module Profligate.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HES
import Partial.Unsafe (unsafePartial)
import Profligate.FlameGraph as FG
import Profligate.Profile.ParseProfile (parseProfFile)
import Profligate.Profile.Profile (filter)
import Profligate.Spinner (spinner)
import Profligate.State (DisplayMode(..), Query(..), State)
import Text.Parsing.StringParser (runParser, ParseError(..))
import Web.Event.Event (EventType(..), preventDefault, stopPropagation)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.Blob (Blob)
import Web.File.File (toBlob)
import Web.File.FileList (item)
import Web.File.FileReader (result, fileReader, readAsText, toEventTarget)
import Web.HTML.Event.DataTransfer (DataTransfer, files)
import Web.HTML.Event.DragEvent (dataTransfer, toEvent)

data Slot = FlameGraphSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

component :: forall m. MonadEffect m => MonadAff m => H.Component HH.HTML Query Unit Void m
component =
    H.parentComponent
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where
    initialState :: State
    initialState =
        { profFile: Nothing
        , parseError: Nothing
        , displayMode: FlameGraph
        , loading: false
        }

    itemStyle :: forall r i. String -> HP.IProp ( style :: String | r ) i
    itemStyle = HP.attr (H.AttrName "style")

    header :: H.ParentHTML Query FG.Query Slot m
    header =
        HH.header
            [ HE.onDragOver (\e -> Just $ H.action (DragOver e))
            , HE.onDrop (\e -> Just $ H.action (UploadFile e))
            ]
            [ HH.h1_ [ HH.text "Profligate" ]
            , HH.div
                [ HP.attr (H.AttrName "class") "dropzone" ]
                [ HH.p_
                    [ HH.text "Plop yer .prof file here" ]
                ]
            ]

    render :: State -> H.ParentHTML Query FG.Query Slot m
    render state =
        HH.div
            [ HP.attr (H.AttrName "class") "container" ]
            [ header
            , HH.main_ $ showMain state
            ]

    showMain :: State -> Array (H.ParentHTML Query FG.Query Slot m)
    showMain { loading, parseError, profFile } 
        | loading = [ spinner ]
        | isJust parseError = showError
        | isJust profFile = [ HH.slot FlameGraphSlot (FG.flameGraph (unsafePartial $ fromJust profFile)) unit absurd ]
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

    showError :: Array (H.ParentHTML Query FG.Query Slot m)
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

    eval :: Query ~> H.ParentDSL State Query FG.Query Slot Void m
    eval = case _ of
        NoOp next -> do
            pure next
        ChangeDisplayMode mode next -> do
            _ <- H.modify $ \state -> state { displayMode = mode }
            pure next
        FileLoaded fr next -> do
            t <- H.liftEffect $ result fr
            let prof = runParser parseProfFile $ unsafeFromForeign t
            _ <- H.modify (\state ->
                case prof of
                    Left (ParseError err) ->
                        state { parseError = Just err, profFile = Nothing, loading = false }
                    Right profFile ->
                        let filteredCostCenterStack = filter (\v -> v.inherited.time > 0.0 || v.inherited.alloc > 0.0) profFile.costCenterStack
                        in state { profFile = Just (profFile { costCenterStack = filteredCostCenterStack }), parseError = Nothing, loading = false })
            pure next
        DragOver e next -> do
            let evt = (toEvent e)
            H.liftEffect $ preventDefault evt
            H.liftEffect $ stopPropagation evt
            pure next
        UploadFile e next -> do
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
            pure next
        where
        bindLoad trgt f = do
            el <- (eventListener f)
            addEventListener (EventType "load") el false trgt

        subthingy trgt fr =
            H.subscribe $ H.eventSource (bindLoad trgt) handleLoad
            where
            handleLoad e = pure $ FileLoaded fr $ HES.Done

getFile :: DataTransfer -> Maybe Blob
getFile dt = do
    fl <- files dt
    f <- item 0 fl
    pure $ toBlob f
