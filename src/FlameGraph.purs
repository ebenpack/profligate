module Profligate.FlameGraph where

import Prelude

import Control.Monad.Gen (chooseInt, oneOf)
import Data.Array as Arr
import Data.Foldable (foldr)
import Data.Int (toNumber, round)
import Data.List (List(..), (:), concatMap, reverse)
import Data.Maybe (Maybe(..))
import Data.String as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types as HVT
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Tree(..), Forest, depth)
import Profligate.State (Query(..), State)

type AnnotatedCostTree =
    { offset :: Number
    , tree :: Tree CostCenterStackCosts
    , color :: Int
    }

flameGraph :: Profile -> State -> H.ComponentHTML Query
flameGraph prof state =
    HH.div
        [ HP.attr (H.AttrName "style") "width:600px; height:400px;" ]
        [ svg
            [ HP.attr (H.AttrName "viewBox") ("0 0 1000 " <> (show totalHeight))
            , HP.attr (H.AttrName "width") "1000"
            , HP.attr (H.AttrName "height") $ show totalHeight
            , HP.attr (H.AttrName "style") "width: 100%; height: auto;"
            ]
            ((doStuff prof) <>
                [ text 
                    [ HP.attr (H.AttrName "x") "10"
                    , HP.attr (H.AttrName "y") "30"
                    , HP.attr (H.AttrName "fill") "black"
                    , HP.attr (H.AttrName "font-size") "30"
                    , HP.attr (H.AttrName "alignment-baseline") "baseline"
                    ] 
                    [ HH.text (state.flameLegend) ]
                ]
            )
        ]
    where
        dep :: Int
        dep = depth prof.costCenterStack

        rowHeight :: Number
        rowHeight = 30.0

        totalWidth :: Number
        totalWidth = 1000.0

        totalHeight :: Number
        totalHeight = (toNumber dep) * rowHeight

        doStuff :: Profile -> Array (H.ComponentHTML Query)
        doStuff p = Arr.fromFoldable $ helper 0 0 0.0 p.costCenterStack

        helper :: Int -> Int -> Number -> Forest CostCenterStackCosts -> List (H.ComponentHTML Query)
        helper col d baseOffset cs = (wrappedRow : descendents)
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
                rowHelper { offset: offsetLeft, tree: (Node { value: c }), color } acc = Arr.cons (drawCostCenter color totalWidth rowHeight offsetLeft offsetTop c) acc
    
                annotateTree :: Tree CostCenterStackCosts -> List AnnotatedCostTree -> List AnnotatedCostTree
                annotateTree c@(Node { value }) Nil = 
                    if value.inherited.time > 0.0 || value.inherited.alloc > 0.0
                    then ({ offset: baseOffset, tree: c, color: col } : Nil)
                    else Nil
                annotateTree c@(Node { value }) (x@({ tree: Node { value: v }, offset: offset, color }) : xs) = 
                    if value.inherited.time > 0.0 || value.inherited.alloc > 0.0
                    then ({ offset: offset + ((v.inherited.time / 100.0) * totalWidth), tree: c, color: nextColor color }) : x : xs
                    else x : xs

                -- TODO: Does rowHelper unreverse this?
                annotatedTree :: List AnnotatedCostTree
                annotatedTree = foldr annotateTree Nil $ reverse cs

                descendents :: List (H.ComponentHTML Query)
                descendents = concatMap descendentsHelper annotatedTree
                    
                descendentsHelper :: AnnotatedCostTree -> List (H.ComponentHTML Query)
                descendentsHelper { offset: offset, tree: (Node { children: t }), color } = helper (nextColor color) (d + 1) offset t

                wrappedRow :: H.ComponentHTML Query
                wrappedRow = g [] row

colors :: Array String
colors =
    [ "#DCF7F3"
    , "#E3AAD6"
    , "#B5D8EB"
    , "#FFBDD8"
    , "#FFFCDD"
    , "#805841"
    , "#FFC8BA"
    , "#D0ECEA"
    , "#93DFB8"
    , "#FFEFD3"
    , "#F5A2A2"
    , "#FC9D9A"
    , "#9FD6D2"
    , "#FFD8D8"
    , "#F9CDAD"
    , "#E8DAFB"
    , "#FFFEE4"
    , "#83AF9B"
    , "#F8DAFB"
    , "#DAFBF8"
    , "#C8C8A9"
    , "#DADDFB"
    ]

nextColor :: Int -> Int
nextColor color = ((color + 1) `mod` (Arr.length colors))

drawCostCenter :: Int -> Number -> Number -> Number -> Number -> CostCenterStackCosts ->  H.ComponentHTML Query
drawCostCenter color totalWidth rowHeight offsetLeft offsetTop cs = 
    let x = show offsetLeft
        y = show offsetTop
        width = show ((cs.inherited.time / 100.0) * totalWidth)
        height = show rowHeight
        eigthHeight = (rowHeight * 0.8)
        fontSize = show eigthHeight
        maxStringLen = (toNumber $ S.length cs.name) * eigthHeight
        name = S.take (round maxStringLen) cs.name
        col = case (Arr.(!!) colors color) of
            Just col' -> col'
            _ -> "red"
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
            , HE.onMouseEnter (\_ -> Just $ H.action (ChangeFlameLegend name))
            , HE.onMouseLeave (\_ -> Just $ H.action (ChangeFlameLegend ""))
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
