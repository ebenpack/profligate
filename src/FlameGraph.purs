module Profligate.FlameGraph where

import Prelude

import Data.Array as Arr
import Data.Char (toCharCode)
import Data.Foldable (foldr)
import Data.Int (toNumber, round)
import Data.List (List(..), (:), concatMap, reverse)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types as HVT
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Tree(..), Forest, depth)


type AnnotatedCostTree =
    { offset :: Number
    , tree :: Tree CostCenterStackCosts
    }

totalWidth :: Number
totalWidth = 1200.0

type State =
    { currentId :: Int
    , selected :: Maybe Int
    , flameLegend :: Maybe CostCenterStackCosts
    }

data Query a =
      NoOp a
    | ChangeFlameLegend (Maybe CostCenterStackCosts) a

data ChildQuery a = Foo a

flameGraph :: forall m. MonadEffect m => MonadAff m => Profile -> H.Component HH.HTML Query Unit Void m
flameGraph prof = H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
    where
        render :: State -> H.ComponentHTML Query
        render state =
            HH.div
                [ HP.attr (H.AttrName "style") "width:600px; height:400px;" ]
                [ (showFlameLegend state.flameLegend)
                , svg
                    [ HP.attr (H.AttrName "viewBox") ("0 0 " <> (show totalWidth) <> " " <> (show totalHeight))
                    , HP.attr (H.AttrName "width") (show totalWidth)
                    , HP.attr (H.AttrName "height") $ show totalHeight
                    , HP.attr (H.AttrName "style") "width: 100%; height: auto;"
                    ]
                    children
                ]


        children = doStuff prof

        showFlameLegend flameLegend =
            HH.div_ [ HH.text t ]
            where
                t = case flameLegend of
                    Nothing -> "Function:"
                    Just flameLegend' ->
                        "Function: " <> flameLegend'.name <>
                            " (" <> show flameLegend'.number <> " samples, " <> show flameLegend'.inherited.time <> "%)"

        initialState :: State
        initialState =
            { currentId: 0
            , selected: Nothing
            , flameLegend: Nothing
            }
        eval :: Query ~> H.ComponentDSL State Query Void m
        eval (NoOp next) = pure next
        eval (ChangeFlameLegend legend next) = do
            _ <- H.modify $ \state -> state { flameLegend = legend }
            pure next

        dep :: Int
        dep = depth prof.costCenterStack

        rowHeight :: Number
        rowHeight = 30.0

        totalHeight :: Number
        totalHeight = (toNumber dep) * rowHeight

        doStuff :: Profile -> Array (H.ComponentHTML Query)
        doStuff p = Arr.fromFoldable $ helper 0 0.0 p.costCenterStack

        helper :: Int -> Number -> Forest CostCenterStackCosts -> List (H.ComponentHTML Query)
        helper d baseOffset cs = (wrappedRow : descendents)
            where
                offsetTop :: Number
                offsetTop = totalHeight - ((toNumber d) * rowHeight) - rowHeight

                calculateLeftOffset :: Number -> Number
                calculateLeftOffset n = ((100.0 - n) / 100.0) * totalWidth

                rowWidth :: Number
                rowWidth = calculateLeftOffset (foldr (\(Node { value: c }) acc -> acc + c.inherited.time) 0.0 cs)

                row :: Array (H.ComponentHTML Query)
                row = Arr.foldr rowHelper [] $ Arr.fromFoldable annotatedTree

                rowHelper :: AnnotatedCostTree -> Array (H.ComponentHTML Query) -> Array (H.ComponentHTML Query)
                rowHelper { offset: offsetLeft, tree: (Node { value: c }) } acc = Arr.cons (drawCostCenter rowHeight offsetLeft offsetTop c) acc

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

                descendents :: List (H.ComponentHTML Query)
                descendents = concatMap descendentsHelper annotatedTree

                descendentsHelper :: AnnotatedCostTree -> List (H.ComponentHTML Query)
                descendentsHelper { offset: offset, tree: (Node { children: t }) } = helper (d + 1) offset t

                wrappedRow :: H.ComponentHTML Query
                wrappedRow = g [] row

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


-- Get a pretty dumb hash of the module and use that to select a color
-- Collisions are likely
getColor :: CostCenterStackCosts -> String
getColor cs =
    let tot = Arr.foldr (\c acc -> acc + (toCharCode c)) 0 (toCharArray cs.mod)
        idx = tot `mod` (Arr.length colors)
    in case (Arr.(!!) colors idx) of
        Just col' -> col'
        _ -> "red"


drawCostCenter :: Number -> Number -> Number -> CostCenterStackCosts ->  H.ComponentHTML Query
drawCostCenter rowHeight offsetLeft offsetTop cs =
    let x = show offsetLeft
        y = show offsetTop
        width = show ((cs.inherited.time / 100.0) * totalWidth)
        height = show rowHeight
        eigthHeight = (rowHeight * 0.8)
        fontSize = show eigthHeight
        maxStringLen = (toNumber $ S.length cs.name) * eigthHeight
        name = S.take (round maxStringLen) cs.name
        col = getColor cs
    in g
        [ ]
        [ rect
            [ HP.attr (H.AttrName "x") x
            , HP.attr (H.AttrName "y") y
            , HP.attr (H.AttrName "rx") "2"
            , HP.attr (H.AttrName "ry") "2"
            , HP.attr (H.AttrName "width") width
            , HP.attr (H.AttrName "height") height
            , HP.attr (H.AttrName "fill") col
            , HP.attr (H.AttrName "stroke") "black"
            , HP.attr (H.AttrName "stroke-width") "2"
            , HE.onClick (\_ -> Just $ H.action (ChangeFlameLegend $ Just cs))
            ]
            []
        ]

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
