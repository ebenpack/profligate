module Profligate.State where

import Data.Eq (class Eq)
import Data.Maybe (Maybe)
import Profligate.Profile.Profile (Profile)
import Web.File.FileReader (FileReader)
import Web.HTML.Event.DragEvent (DragEvent)


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