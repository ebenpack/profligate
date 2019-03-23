module Profligate.State where

import Data.Maybe (Maybe)
import Profligate.Profile.Profile (Profile)
import Web.File.FileReader (FileReader)
import Web.HTML.Event.DragEvent (DragEvent)


data DisplayMode = FlameGraph

data Query a =
      UploadFile DragEvent a
    | FileLoaded FileReader a
    | DragOver DragEvent a
    | ChangeDisplayMode DisplayMode a
    | NoOp a

type State =
    { profFile :: Maybe Profile
    , parseError :: Maybe String
    , displayMode :: DisplayMode
    }