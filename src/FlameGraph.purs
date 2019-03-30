module Profligate.FlameGraph where

import Prelude hiding (div)

import Data.Array as Arr
import Data.Char (toCharCode)
import Data.Foldable (foldr)
import Data.Int (toNumber, round)
import Data.List (List(..), (:), concatMap, reverse)
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.String.CodeUnits (toCharArray)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML (div)
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types as HVT
import Math (floor)
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Tree(..), Forest, depth)

type ChildSlots = ()

type Slot = H.Slot Query Void

type AnnotatedCostTree =
    { offset :: Number
    , tree :: Tree CostCenterStackCosts
    }

totalWidth :: Number
totalWidth = 1200.0

strokeWidth :: Int
strokeWidth = 2

type State =
    { currentId :: Int
    , selected :: Maybe Int
    , flameLegend :: Maybe CostCenterStackCosts
    }

data Query a = NoOpQ a

data Action =
      NoOp
    | ChangeFlameLegend (Maybe CostCenterStackCosts)

flameGraph :: forall f i o m. MonadEffect m => MonadAff m => Profile -> H.Component HH.HTML f i o m
flameGraph prof = H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = eval }
    }
    where
        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div
                [ HP.attr (H.AttrName "style") "width:100%;" ]
                [ svg
                    [ HP.attr (H.AttrName "viewBox") ("0 0 " <> (show totalWidth) <> " " <> (show totalHeight))
                    , HP.attr (H.AttrName "width") (show totalWidth)
                    , HP.attr (H.AttrName "height") $ show totalHeight
                    , HP.attr (H.AttrName "style") "width: 100%; height: auto;"
                    ]
                    [ (showFlameLegend state.flameLegend)
                    , children
                    ]
                ]


        children = doStuff prof

        showFlameLegend cs =
            text
                [ HP.attr (H.AttrName "y") (show (rowHeight * 2.0))
                , HP.attr (H.AttrName "x") (show (totalWidth * 0.02))
                , HP.attr (H.AttrName "font-size") (show rowHeight)
                ]
                [ HH.text t ]
            where
            t = maybe "" displayStackInfo cs

        initialState :: State
        initialState =
            { currentId: 0
            , selected: Nothing
            , flameLegend: Nothing
            }

        eval :: Action -> H.HalogenM State Action ChildSlots o m Unit
        eval NoOp = pure unit
        eval (ChangeFlameLegend legend) = do
            _ <- H.modify $ \state -> state { flameLegend = legend }
            pure unit

        dep :: Int
        dep = depth prof.costCenterStack

        rowHeight :: Number
        rowHeight = 30.0

        totalHeight :: Number
        totalHeight = (toNumber dep) * rowHeight

        doStuff :: forall b. Profile -> H.ComponentHTML Action b m
        doStuff p = g [] (Arr.fromFoldable $ helper 0 0.0 p.costCenterStack)

        helper :: forall b. Int -> Number -> Forest CostCenterStackCosts -> List (H.ComponentHTML Action b m)
        helper d baseOffset cs = (wrappedRow : descendents)
            where
                offsetTop :: Number
                offsetTop = totalHeight - ((toNumber d) * rowHeight) - rowHeight

                calculateLeftOffset :: Number -> Number
                calculateLeftOffset n = ((100.0 - n) / 100.0) * totalWidth

                rowWidth :: Number
                rowWidth = calculateLeftOffset (foldr (\(Node { value: c }) acc -> acc + c.inherited.time) 0.0 cs)

                row :: Array (H.ComponentHTML Action b m)
                row = Arr.foldr rowHelper [] $ Arr.fromFoldable annotatedTree

                rowHelper :: AnnotatedCostTree -> Array (H.ComponentHTML Action b m) -> Array (H.ComponentHTML Action b m)
                rowHelper { offset: offsetLeft, tree: (Node { value: c }) } acc = Arr.cons (drawCostCenter offsetLeft offsetTop c) acc

                annotateTree :: Tree CostCenterStackCosts -> List AnnotatedCostTree -> List AnnotatedCostTree
                annotateTree c@(Node { value }) Nil =
                    if value.inherited.time > 0.0 || value.inherited.alloc > 0.0
                    then ({ offset: baseOffset, tree: c } : Nil)
                    else Nil
                annotateTree c@(Node { value }) (x@({ tree: Node { value: v }, offset: offset }) : xs) =
                    if value.inherited.time > 0.0 || value.inherited.alloc > 0.0
                    then ({ offset: offset + ((v.inherited.time / 100.0) * totalWidth), tree: c }) : x : xs
                    else x : xs

                -- TODO: Does rowHelper unreverse this?
                annotatedTree :: List AnnotatedCostTree
                annotatedTree = foldr annotateTree Nil $ reverse cs

                descendents :: List (H.ComponentHTML Action b m)
                descendents = concatMap descendentsHelper annotatedTree

                descendentsHelper :: AnnotatedCostTree -> List (H.ComponentHTML Action b m)
                descendentsHelper { offset: offset, tree: (Node { children: t }) } = helper (d + 1) offset t

                wrappedRow :: H.ComponentHTML Action b m
                wrappedRow = g [] row

        drawCostCenter :: forall b. Number -> Number -> CostCenterStackCosts ->  H.ComponentHTML Action b m
        drawCostCenter offsetLeft offsetTop cs =
            let x = show offsetLeft
                y = show offsetTop
                width = show $ floor ((cs.inherited.time / 100.0) * totalWidth)
                height = show rowHeight
                eigthHeight = (rowHeight * 0.8)
                fontSize = show eigthHeight
                maxStringLen = (toNumber $ S.length cs.name) * eigthHeight
                name = S.take (round maxStringLen) cs.name
                col = getColor cs
            in g
                [ HE.onMouseEnter (\_ -> Just (ChangeFlameLegend $ Just cs))
                , HE.onMouseLeave (\_ -> Just (ChangeFlameLegend Nothing))
                , HP.attr (H.AttrName "class") "stack"]
                [ rect
                    [ HP.attr (H.AttrName "x") x
                    , HP.attr (H.AttrName "y") y
                    , HP.attr (H.AttrName "width") width
                    , HP.attr (H.AttrName "height") height
                    , HP.attr (H.AttrName "fill") col
                    , HP.attr (H.AttrName "stroke-width") (show strokeWidth)
                    ]
                    [ title
                        []
                        [ text [] [ HH.text $ displayStackInfo cs ]]
                    ]
                , foreignObject
                    [ HP.attr (H.AttrName "x") x
                    , HP.attr (H.AttrName "y") y
                    , HP.attr (H.AttrName "width") width
                    , HP.attr (H.AttrName "height") height
                    , HP.attr (H.AttrName "pointer-events") "none"
                    ]
                    [ div
                        [ HP.attr (H.AttrName "style") ("font-size:" <> show (rowHeight * 0.7) <> "px;height" <> height<> ";")
                        , HP.attr (H.AttrName "pointer-events") "none"
                        ]
                        [ HH.text $ cs.name ]
                    ]
                ]

colors :: Array String
colors =
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

displayStackInfo :: CostCenterStackCosts -> String
displayStackInfo cs =
    "Function: " <> cs.name <>
        " (" <> show cs.entries <> " entries, " <> show cs.inherited.time <> "%)"


-- Get a pretty dumb hash of the module and use that to select a color
-- Collisions are likely
getColor :: CostCenterStackCosts -> String
getColor cs =
    let tot = Arr.foldr (\c acc -> acc + (toCharCode c)) 0 (toCharArray cs.mod)
        idx = tot `mod` (Arr.length colors)
    in case (Arr.(!!) colors idx) of
        Just col' -> col'
        _ -> "red"

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

defs :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
defs props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "defs") props children

clipPath :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
clipPath props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "clipPath") props children

use :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
use props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "use") props children

g :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
g props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "g") props children

foreignObject :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
foreignObject props children =
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "foreignObject") props children
