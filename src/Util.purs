module Profligate.Util where

import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types as HVT

svg :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
svg props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg")  (HVT.ElemName "svg") props children

rect :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
rect props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "rect") props children

text :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
text props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "text") props children

title :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
title props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "title") props children

g :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
g props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "g") props children

foreignObject :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
foreignObject props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "foreignObject") props children

pastelColorSet :: Array String
pastelColorSet =
    [ "#B5D8EB"
    , "#DCF7F3"
    , "#E3AAD6"
    , "#F5A2A2"
    , "#F8DAFB"
    , "#F9CDAD"
    , "#FFBDD8"
    , "#FFC8BA"
    , "#FFD8D8"
    , "#FFFCDD"
    ]

largeColorSet :: Array String
largeColorSet =
    [ "#FFFF00"
    , "#1CE6FF"
    , "#FF34FF"
    , "#FF4A46"
    , "#008941"
    , "#006FA6"
    , "#A30059"
    , "#FFDBE5"
    , "#7A4900"
    , "#0000A6"
    , "#63FFAC"
    , "#B79762"
    , "#004D43"
    , "#8FB0FF"
    , "#997D87"
    , "#5A0007"
    , "#809693"
    , "#FEFFE6"
    , "#1B4400"
    , "#4FC601"
    , "#3B5DFF"
    , "#4A3B53"
    , "#FF2F80"
    , "#61615A"
    , "#BA0900"
    , "#6B7900"
    , "#00C2A0"
    , "#FFAA92"
    , "#FF90C9"
    , "#B903AA"
    , "#D16100"
    , "#DDEFFF"
    , "#000035"
    , "#7B4F4B"
    , "#A1C299"
    , "#300018"
    , "#0AA6D8"
    , "#013349"
    , "#00846F"
    , "#372101"
    , "#FFB500"
    , "#C2FFED"
    , "#A079BF"
    , "#CC0744"
    , "#C0B9B2"
    , "#C2FF99"
    , "#001E09"
    , "#00489C"
    , "#6F0062"
    , "#0CBD66"
    , "#EEC3FF"
    , "#456D75"
    , "#B77B68"
    , "#7A87A1"
    , "#788D66"
    , "#885578"
    , "#FAD09F"
    , "#FF8A9A"
    , "#D157A0"
    , "#BEC459"
    , "#456648"
    , "#0086ED"
    , "#886F4C"
    , "#34362D"
    , "#B4A8BD"
    , "#00A6AA"
    , "#452C2C"
    , "#636375"
    , "#A3C8C9"
    , "#FF913F"
    , "#938A81"
    , "#575329"
    , "#00FECF"
    , "#B05B6F"
    , "#8CD0FF"
    , "#3B9700"
    , "#04F757"
    , "#C8A1A1"
    , "#1E6E00"
    , "#7900D7"
    , "#A77500"
    , "#6367A9"
    , "#A05837"
    , "#6B002C"
    , "#772600"
    , "#D790FF"
    , "#9B9700"
    , "#549E79"
    , "#FFF69F"
    , "#201625"
    , "#72418F"
    , "#BC23FF"
    , "#99ADC0"
    , "#3A2465"
    , "#922329"
    , "#5B4534"
    , "#FDE8DC"
    , "#404E55"
    , "#0089A3"
    , "#CB7E98"
    , "#A4E804"
    , "#324E72"
    , "#6A3A4C"
    ]