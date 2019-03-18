module Profligate.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HES
import Profligate.Profile.ParseProfile (parseProfFile)
import Profligate.State (DisplayMode(..), Query(..), State)
import Profligate.FlameGraph (flameGraph)
import Text.Parsing.StringParser (runParser, ParseError(..))
import Web.Event.Event (EventType(..), preventDefault, stopPropagation)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.Blob (Blob)
import Web.File.File (toBlob)
import Web.File.FileList (item)
import Web.File.FileReader (result, fileReader, readAsText, toEventTarget)
import Web.HTML.Event.DataTransfer (DataTransfer, files)
import Web.HTML.Event.DragEvent (dataTransfer, toEvent)

component :: forall m. MonadEffect m => MonadAff m => H.Component HH.HTML Query Unit Void m
component =
    H.component
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where
    initialState :: State
    initialState =
        { profFile: Nothing
        , parseError : Nothing
        , displayMode : FlameGraph
        }

    itemStyle :: forall r i. String -> HP.IProp ( style :: String | r ) i
    itemStyle = HP.attr (H.AttrName "style")

    dropzone :: H.ComponentHTML Query
    dropzone =
        HH.div
            [ HE.onDragOver (\e -> Just $ H.action (DragOver e))
            , HE.onDrop (\e -> Just $ H.action (UploadFile e))
            , itemStyle "width: 100px; height: 100px; border: 1px solid black;"
            ]
            [ HH.text "Plop yer prof file here" ]

    render :: State -> H.ComponentHTML Query
    render state =
        HH.div_
            [ HH.h1_ [ HH.text "Profligate" ]
            , dropzone
            , HH.div_
                [ HH.h2_ [ HH.text "DEBUG" ]
                -- , HH.div_ [ HH.text "Profile: ", profileDebug ]
                -- , HH.div_ [ HH.text "Error: ", errorDebug ]
                , case state.profFile of
                    Just p -> flameGraph p
                    Nothing -> HH.div_ [ HH.text "" ]
                ]
            ]
        where
        profileDebug =
            case state.profFile of
                Nothing -> HH.text "NOTHING"
                Just p -> HH.text $ show p
        errorDebug =
            case state.parseError of
                Nothing -> HH.text "OK"
                Just err -> HH.text err

    eval :: Query ~> H.ComponentDSL State Query Void m
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
                    Left (ParseError err) -> state { parseError = Just err, profFile = Nothing }
                    Right profFile -> state { profFile = Just profFile, parseError = Nothing })
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
            let trans = dataTransfer e
                blob = getFile trans
            case blob of
                Nothing -> pure unit
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
            -- handleLoad
            handleLoad e = pure $ FileLoaded fr $ HES.Done

getFile :: DataTransfer -> Maybe Blob
getFile dt = do
    fl <- files dt
    f <- item 0 fl
    pure $ toBlob f
