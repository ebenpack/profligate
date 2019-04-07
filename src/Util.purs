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
