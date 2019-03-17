module Profligate.FlameGraph where

import Prelude

import Data.Array ((:), fromFoldable, concatMap)
import Data.Foldable (foldr)
import Data.Functor (map)
import Data.Int (round)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types as HVT
import Halogen.VDom.DOM.Prop as HVDP
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Tree(..), Forest)
import Profligate.State (Query)

import Debug.Trace

flameGraph :: Profile -> H.ComponentHTML Query
flameGraph p =
    HH.div
    [ HP.attr (H.AttrName "style") "width:600px; height:400px;" ]
    [ svg
        [ HP.attr (H.AttrName "viewBox") "0 0 100 100"
        , HP.attr (H.AttrName "width") "100"
        , HP.attr (H.AttrName "height") "100"
        , HP.attr (H.AttrName "style") "width: 100%; height: auto;"
        ] 
        (doStuff p)
    ]

drawCostCenter :: Number -> CostCenterStackCosts -> Array (H.ComponentHTML Query)
drawCostCenter rowWidth cs = 
    [ rect
            [ HP.attr (H.AttrName "x") "2"
            , HP.attr (H.AttrName "y") "7"
            , HP.attr (H.AttrName "width") ((show $ round cs.inherited.time))
            , HP.attr (H.AttrName "height") "10"
            , HP.attr (H.AttrName "fill") "red"
            ]
            []
    , text 
        [ HP.attr (H.AttrName "x") "0"
        , HP.attr (H.AttrName "y") "0"
        , HP.attr (H.AttrName "fill") "black"
        , HP.attr (H.AttrName "style") "font-size: 5px;"
        ] 
        [ HH.text cs.name ]
    ]

doStuff :: Profile -> Array (H.ComponentHTML Query)
doStuff p = helper 0 p.costCenterStack
    where
    -- TODO: depth
    helper :: Int -> Forest CostCenterStackCosts -> Array (H.ComponentHTML Query)
    helper n cs = (wrappedRow : descentdents)
        where
            rowWidth :: Number
            rowWidth = foldr (\(Node { value: c }) acc -> 
                let zzz = trace c (\_ -> 's') in acc + c.inherited.time) 0.0 cs
            row :: Array (H.ComponentHTML Query)
            row = foldr (\(Node { value: c }) acc -> (drawCostCenter rowWidth c) <> acc) [] cs
            descentdents :: Array (H.ComponentHTML Query)
            descentdents = concatMap (\(Node { children: t }) -> (helper (n + 1) t)) (fromFoldable cs)
            wrappedRow :: H.ComponentHTML Query
            wrappedRow = 
                let x = show (100.0 - rowWidth)
                    y = show (n * 10)
                in g 
                    [ HP.attr (H.AttrName "transform") ("translate(" <> x <> "," <> y <> ")")
                    , HP.attr (H.AttrName "width") (show rowWidth)
                    , HP.attr (H.AttrName "height") "10"
                    , HP.attr (H.AttrName "fill") "red"
                    ] row

svg :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
svg props children = 
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg")  (HVT.ElemName "svg") props children

rect :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
rect props children = 
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "rect") props children

text :: forall r p i. Array (HP.IProp r i) -> Array (HHC.HTML p i) -> HHC.HTML p i
text props children = 
    HH.elementNS (HVT.Namespace "http://www.w3.org/2000/svg") (HVT.ElemName "text") props children


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