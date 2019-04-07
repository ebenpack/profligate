module Profligate.Types where

import Prelude (class Eq, class Show)

data DisplayMode = 
    FlameGraph
  | TreeViz

derive instance eqDisplayMode :: Eq DisplayMode

data AnalysisMode = 
    Time
  | Alloc

derive instance eqAnalysisMode :: Eq AnalysisMode

instance showAnalysisMode :: Show AnalysisMode where
  show Time = "Time"
  show Alloc = "Alloc"