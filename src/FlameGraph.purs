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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (floor)
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Tree(..), Forest, depth)
import Profligate.Types (AnalysisMode(..))
import Profligate.Util (svg, rect, text, title, g, foreignObject, pastelColorSet)

type Input = AnalysisMode

type Slot = H.Slot Query Void

type ChildSlots = ()

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
    , analysisMode :: AnalysisMode
    }

data Query a = NoOpQ a

data Action =
      NoOp
    | SetAnalysisMode AnalysisMode
    | ChangeFlameLegend (Maybe CostCenterStackCosts)

flameGraph :: forall f o m. MonadEffect m => MonadAff m => Profile -> AnalysisMode -> H.Component HH.HTML f Input o m
flameGraph prof analysisMode = H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = eval
        , receive = \am -> Just $ SetAnalysisMode am
        }
    }
    where
        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div
                [ HP.attr (H.AttrName "class") "flamegraph"
                , HP.attr (H.AttrName "style") "width:100%;" ]
                [ svg
                    [ HP.attr (H.AttrName "viewBox") ("0 0 " <> (show totalWidth) <> " " <> (show totalHeight))
                    , HP.attr (H.AttrName "width") (show totalWidth)
                    , HP.attr (H.AttrName "height") $ show totalHeight
                    , HP.attr (H.AttrName "style") "width: 100%; height: auto;"
                    ]
                    [ (showFlameLegend state.analysisMode state.flameLegend)
                    , children state.analysisMode
                    ]
                ]

        getCost :: AnalysisMode -> CostCenterStackCosts -> Number
        getCost am cs = case am of
            Time -> cs.inherited.time
            Alloc -> cs.inherited.alloc

        children :: forall b. AnalysisMode -> H.ComponentHTML Action b m
        children am = doStuff am prof

        displayStackInfo :: AnalysisMode -> CostCenterStackCosts -> String
        displayStackInfo am cs =
            "Function: " <> cs.name <>
                " (" <> show cs.entries <> " entries, " <> show (getCost am cs) <> "%)"

        showFlameLegend :: forall b. AnalysisMode -> Maybe CostCenterStackCosts -> H.ComponentHTML Action b m
        showFlameLegend am cs =
            text
                [ HP.attr (H.AttrName "y") (show (rowHeight * 2.0))
                , HP.attr (H.AttrName "x") (show (totalWidth * 0.02))
                , HP.attr (H.AttrName "font-size") (show rowHeight)
                ]
                [ HH.text t ]
            where
            t = maybe "" (displayStackInfo am) cs

        initialState :: State
        initialState =
            { currentId: 0
            , selected: Nothing
            , flameLegend: Nothing
            , analysisMode: analysisMode
            }

        eval :: Action -> H.HalogenM State Action ChildSlots o m Unit
        eval NoOp = pure unit
        eval (SetAnalysisMode mode) = do
            _ <- H.modify $ \state -> state { analysisMode = mode }
            pure unit
        eval (ChangeFlameLegend legend) = do
            _ <- H.modify $ \state -> state { flameLegend = legend }
            pure unit

        dep :: Int
        dep = depth prof.costCenterStack

        rowHeight :: Number
        rowHeight = 30.0

        totalHeight :: Number
        totalHeight = (toNumber dep) * rowHeight

        doStuff :: forall b. AnalysisMode -> Profile -> H.ComponentHTML Action b m
        doStuff am p = g [] (Arr.fromFoldable $ helper am 0 0.0 p.costCenterStack)

        helper :: forall b. AnalysisMode -> Int -> Number -> Forest CostCenterStackCosts -> List (H.ComponentHTML Action b m)
        helper am d baseOffset cs = (wrappedRow : descendents)
            where
                offsetTop :: Number
                offsetTop = totalHeight - ((toNumber d) * rowHeight) - rowHeight

                calculateLeftOffset :: Number -> Number
                calculateLeftOffset n = ((100.0 - n) / 100.0) * totalWidth

                row :: Array (H.ComponentHTML Action b m)
                row = Arr.foldr rowHelper [] $ Arr.fromFoldable annotatedTree

                rowHelper :: AnnotatedCostTree -> Array (H.ComponentHTML Action b m) -> Array (H.ComponentHTML Action b m)
                rowHelper { offset: offsetLeft, tree: (Node { value: c }) } acc = Arr.cons (drawCostCenter am offsetLeft offsetTop c) acc

                annotateTree :: Tree CostCenterStackCosts -> List AnnotatedCostTree -> List AnnotatedCostTree
                annotateTree c@(Node { value }) Nil =
                    { offset: baseOffset, tree: c } : Nil
                annotateTree c@(Node { value }) (x@({ tree: Node { value: v }, offset: offset }) : xs) =
                    { offset: offset + (((getCost am v) / 100.0) * totalWidth), tree: c } : x : xs

                annotatedTree :: List AnnotatedCostTree
                annotatedTree = foldr annotateTree Nil $ reverse cs

                descendents :: List (H.ComponentHTML Action b m)
                descendents = concatMap descendentsHelper annotatedTree

                descendentsHelper :: AnnotatedCostTree -> List (H.ComponentHTML Action b m)
                descendentsHelper { offset: offset, tree: (Node { children: t }) } = helper am (d + 1) offset t

                wrappedRow :: H.ComponentHTML Action b m
                wrappedRow = g [] row

        drawCostCenter :: forall b. AnalysisMode -> Number -> Number -> CostCenterStackCosts ->  H.ComponentHTML Action b m
        drawCostCenter am offsetLeft offsetTop cs =
            let x = show offsetLeft
                y = show offsetTop
                width = show $ floor (((getCost am cs) / 100.0) * totalWidth)
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
                        [ text [] [ HH.text $ displayStackInfo am cs ]]
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


-- Get a pretty dumb hash of the module and use that to select a color
-- Collisions are likely
getColor :: CostCenterStackCosts -> String
getColor cs =
    let tot = Arr.foldr (\c acc -> acc + (toCharCode c)) 0 (toCharArray cs.mod)
        idx = tot `mod` (Arr.length pastelColorSet)
    in case (Arr.(!!) pastelColorSet idx) of
        Just col' -> col'
        _ -> "red"
