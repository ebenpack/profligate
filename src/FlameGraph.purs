module Profligate.FlameGraph where

import Prelude

import Data.Array as Arr
import Data.Foldable (foldr)
import Data.Int (toNumber, round)
import Data.List (List(..), (:), concatMap, reverse)
import Data.String as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types as HVT
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Tree(..), Forest, depth)
import Profligate.State (Query)

type AnnotatedCostTree =
    { offset :: Number
    , tree :: Tree CostCenterStackCosts
    }

flameGraph :: Profile -> H.ComponentHTML Query
flameGraph prof =
    HH.div
    [ HP.attr (H.AttrName "style") "width:600px; height:400px;" ]
    [ svg
        [ HP.attr (H.AttrName "viewBox") ("0 0 1000 " <> (show totalHeight))
        , HP.attr (H.AttrName "width") "1000"
        , HP.attr (H.AttrName "height") $ show totalHeight
        , HP.attr (H.AttrName "style") "width: 100%; height: auto;"
        ]
        ([
            defs
                []
                [ rect
                    [ HP.attr (H.AttrName "id") "rect"
                    , HP.attr (H.AttrName "width") "100%"
                    , HP.attr (H.AttrName "width") "100%"
                    , HP.attr (H.AttrName "fill") "none"
                    ]
                    []
                , clipPath
                    [ HP.attr (H.AttrName "id") "clip" ]
                    [ use 
                        [ HP.attr (H.AttrName "xlink:href") "#rect" ]
                        []
                    ]
                ]
        ] <> (doStuff prof))
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
                rowHelper { offset: offsetLeft, tree: (Node { value: c }) } acc = Arr.cons (drawCostCenter totalWidth rowHeight offsetLeft offsetTop c) acc
    
                annotateTree :: Tree CostCenterStackCosts -> List AnnotatedCostTree -> List AnnotatedCostTree
                annotateTree c Nil = ({ offset: baseOffset, tree: c } : Nil)
                annotateTree c (x@({ tree: Node { value: v }, offset: offset }) : xs) = ({ offset: offset + ((v.inherited.time / 100.0) * totalWidth), tree: c }) : x : xs

                -- TODO: Does rowHelper unreverse this?
                annotatedTree :: List AnnotatedCostTree
                annotatedTree = foldr annotateTree Nil $ reverse cs

                descendents :: List (H.ComponentHTML Query)
                descendents = concatMap descendentsHelper annotatedTree
                    
                descendentsHelper :: AnnotatedCostTree -> List (H.ComponentHTML Query)
                descendentsHelper { offset: offset, tree: (Node { children: t }) } = helper (d + 1) offset t

                wrappedRow :: H.ComponentHTML Query
                wrappedRow = g [] row

drawCostCenter :: Number -> Number -> Number -> Number -> CostCenterStackCosts ->  H.ComponentHTML Query
drawCostCenter totalWidth rowHeight offsetLeft offsetTop cs = 
    let x = show offsetLeft
        y = show offsetTop
        width = show ((cs.inherited.time / 100.0) * totalWidth)
        height = show rowHeight
        eigthHeight = (rowHeight * 0.8)
        fontSize = show eigthHeight
        maxStringLen = (toNumber $ S.length cs.name) * eigthHeight
        name = S.take (round maxStringLen) cs.name
    in g
        [  ]
        [ rect
                [ HP.attr (H.AttrName "x") x
                , HP.attr (H.AttrName "y") y
                , HP.attr (H.AttrName "rx") "2"
                , HP.attr (H.AttrName "ry") "2"
                , HP.attr (H.AttrName "width") width
                , HP.attr (H.AttrName "height") height
                , HP.attr (H.AttrName "fill") "red"
                , HP.attr (H.AttrName "stroke") "black"
                , HP.attr (H.AttrName "stroke-width") "2"
                ]
                []
        , text 
            [ HP.attr (H.AttrName "x") (show (offsetLeft + (totalWidth * 0.01)))
            , HP.attr (H.AttrName "y") (show (offsetTop + eigthHeight))
            , HP.attr (H.AttrName "fill") "black"
            , HP.attr (H.AttrName "font-size") fontSize
            , HP.attr (H.AttrName "alignment-baseline") "baseline"
            ] 
            [ HH.text name ]
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


-- newtype Tree a = Node { value :: a
--                       , children :: Forest a 
--                       }

-- type Forest a = List (Tree a)

-- derive instance eqTree :: (Eq a) => Eq (Tree a)

-- instance showTree :: (Show a) => Show (Tree a) where
--   show (Node { value: a, children: t }) = "Node " <> show a <> show t

-- type TotalTime =
--     { time :: Number
--     , ticks :: Int
--     , interval :: Int
--     , processors :: Int
--     }

-- type CostCenter l =
--     { name :: String
--     , mod :: String
--     , src :: String
--     , ticks :: Maybe Int
--     , bytes :: Maybe Int
--     | l
--     }

-- type PerCostCenterCosts = CostCenter (time :: Number , alloc :: Number)

-- type CostCenterStackCosts = CostCenter
--     ( number :: Int
--     , entries :: Int
--     , individual :: { time :: Number , alloc :: Number }
--     , inherited :: { time :: Number , alloc :: Number }
--     )

-- type Profile =
--     { timestamp :: DateTime
--     , title     :: String
--     , totalTime :: TotalTime
--     , totalAlloc :: Int
--     , perCostCenterCosts :: List PerCostCenterCosts
--     , costCenterStack :: Forest CostCenterStackCosts
--     }