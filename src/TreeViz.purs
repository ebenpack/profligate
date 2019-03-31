module Profligate.TreeViz where

import Debug.Trace
import Prelude

import Data.Array as Arr
import Data.Foldable (foldr)
import Data.List (List(..), (:), concatMap, reverse, filter, null, snoc)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML (div)
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types as HVT
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Forest, Tree(..))
import Web.HTML.HTMLAreaElement (coords)

type Slot = H.Slot Query Void

type TreeCoords = List Int

type State =
    { costCenterStack :: AnnotatedCostCenterStackTree
    , focus :: TreeCoords
    }

type ChildSlots = ()

data Query a =
      NoOpQ a

data Action =
      NoOp
    | Collapse TreeCoords
    | Uncollapse TreeCoords
    | Focus TreeCoords

type AnnotatedCostCenterStackCosts =
    { depth :: Int
    , index :: Int
    , collapsed :: Boolean
    , stack :: CostCenterStackCosts
    }

type AnnotatedCostCenterStackTree = Forest AnnotatedCostCenterStackCosts

annotatedTree :: Int -> Forest CostCenterStackCosts -> AnnotatedCostCenterStackTree
annotatedTree dep cs = reverse $ foldr (annotateTree dep) Nil $ reverse cs

annotateTree :: Int -> Tree CostCenterStackCosts -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
annotateTree dep (Node { value, children }) Nil =
    (Node{ value: {index: 0, depth: dep, collapsed: true, stack: value}, children: (annotatedTree (dep + 1) children) }) : Nil
annotateTree dep (Node { value, children }) (x@(Node{ value: { index } }) : xs) =
    (Node{ value: {index: index + 1, depth: dep, collapsed: true, stack: value}, children: (annotatedTree (dep + 1) children) }) : x : xs


treeViz :: forall f i o m. MonadEffect m => MonadAff m => Profile -> H.Component HH.HTML f i o m
treeViz { costCenterStack } = H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = eval }
    }
    where

    lapse :: Boolean -> TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    lapse b coords stack = setin setCollapsed coords stack
        where
        setCollapsed (Node{ value, children}) = (Node{ value: value { collapsed = b }, children })

    setin :: (Tree AnnotatedCostCenterStackCosts -> Tree AnnotatedCostCenterStackCosts) -> TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    setin f (0:Nil) (y:ys) = f y : ys
    setin f (0:ns) (Node{ value, children}:ys) = (Node{ value, children: setin f ns children }):ys
    setin f (m:ns) (y:ys) = y : setin f ((m-1):ns) ys
    setin _ _ xs = xs

    getin :: TreeCoords -> AnnotatedCostCenterStackTree -> Maybe CostCenterStackCosts
    getin (0:Nil) ((Node{ value }):ys) = Just value.stack
    getin (0:ns) (Node{ value, children}:ys) = getin ns children
    getin (m:ns) (y:ys) = getin ((m-1):ns) ys
    getin _ xs = Nothing

    uncollapse :: TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    uncollapse = lapse false -- TODO: Collapse childrens

    collapse :: TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    collapse = lapse true

    startingTree :: AnnotatedCostCenterStackTree
    startingTree = annotatedTree 0 costCenterStack

    initialState :: State
    initialState = { costCenterStack: startingTree, focus: Nil }

    render :: State -> H.ComponentHTML Action ChildSlots m
    render state =
        HH.div
            [ HP.attr (H.AttrName "class") "treeviz" ]
            [ HH.div
                [ HP.attr (H.AttrName "class") "left" ]
                [ HH.div
                    [ HP.attr (H.AttrName "class") "col" ]
                    [ HH.div
                        [ HP.attr (H.AttrName "class") "info" ]
                        [ showStats state.focus state.costCenterStack ]
                    , HH.div
                        [ HP.attr (H.AttrName "class") "tree" ]
                        [ showTree state.focus state.costCenterStack ]
                    ]
                ]
            , HH.div
                [ HP.attr (H.AttrName "class") "right" ]
                []
            ]

    showStats :: TreeCoords -> AnnotatedCostCenterStackTree -> H.ComponentHTML Action ChildSlots m
    showStats coords cs =
        case getin coords cs of
            Nothing -> HH.div [] []
            Just stats -> 
                HH.div_
                    [ HH.div_ [ HH.h2_ [ HH.text stats.name ] ]
                    , HH.div_ [ HH.text $ "Module: " <> stats.mod ]
                    , HH.div_ [ HH.text $ "Entries: " <> show stats.entries ]
                    , HH.div_ [ HH.text $ "Time: " <> show stats.individual.time ]
                    , HH.div_ [ HH.text $ "Alloc: " <> show stats.individual.alloc ]
                    , HH.div_ [ HH.text $ "Time (inherited): " <> show stats.inherited.alloc ]
                    , HH.div_ [ HH.text $ "Alloc (inherited): " <> show stats.inherited.alloc ]
                    ]

    showTree :: TreeCoords -> AnnotatedCostCenterStackTree -> H.ComponentHTML Action ChildSlots m
    showTree focus cs = HH.div [] (Arr.fromFoldable $ showTreeHelper focus  Nil cs)

    showTreeHelper :: TreeCoords -> TreeCoords -> AnnotatedCostCenterStackTree -> Array (H.ComponentHTML Action ChildSlots m)
    showTreeHelper focus coords cs = row
        where
            row :: Array (H.ComponentHTML Action ChildSlots m)
            row =
                [ HH.div
                    [ HP.attr (H.AttrName "class") ("stack" <> (if null coords then "" else " nested")) ]
                    (Arr.foldr rowHelper [] $ Arr.fromFoldable cs)
                ]

            rowHelper :: Tree AnnotatedCostCenterStackCosts -> Array (H.ComponentHTML Action ChildSlots m) -> Array (H.ComponentHTML Action ChildSlots m)
            rowHelper c@(Node{ value: { index } }) acc = (drawTreeView focus (snoc coords index) c) <> acc

    drawTreeView :: TreeCoords -> TreeCoords -> Tree AnnotatedCostCenterStackCosts -> Array (H.ComponentHTML Action ChildSlots m)
    drawTreeView focus coords (Node{ value: { stack, collapsed, depth, index }, children }) =
        [ HH.div_
            ([ HH.pre_
                [ drawTitle (stack.name <> ": " <> show stack.individual.time) ] ] <> rest)
        ]
        where
        rest =
            if collapsed
            then []
            else showTreeHelper focus coords children
        drawTitle name =
            if null children then
                HH.span
                    spanAttrs
                    [ HH.span
                        [ HP.attr (H.AttrName "class") "arrow" ]
                        [ ]
                    , HH.text name
                    ]
            else if collapsed -- TODO: If uncollapsable
                then
                    HH.span
                        spanAttrs
                        [ HH.span
                            [ HP.attr (H.AttrName "class") "arrow"
                            , HE.onClick (\_ -> trace (show coords) \_ -> Just (Uncollapse coords))
                            ]
                            [ HH.text "▷" ]
                        , HH.text name
                        ]
                else
                    HH.span
                        spanAttrs
                        [ HH.span
                            [ HP.attr (H.AttrName "class") "arrow"
                            , HE.onClick (\_ -> Just (Collapse coords))
                            ]
                            [ HH.text "▽" ]
                        , HH.text name
                        ]
                where
                spanAttrs = 
                    [ HE.onClick (\_ -> Just (Focus coords))
                    , HP.attr (H.AttrName "class") (if focus == coords then "selected" else "")
                    ]

    eval :: Action -> H.HalogenM State Action ChildSlots o m Unit
    eval NoOp = pure unit
    eval (Uncollapse coords) = do
        _ <- H.modify $ \state ->
            state { costCenterStack = uncollapse coords state.costCenterStack, focus = coords }
        pure unit
    eval (Collapse coords) = do
        _ <- H.modify $ \state ->
            state { costCenterStack = collapse coords state.costCenterStack, focus = coords }
        pure unit
    eval (Focus coords) = do
        _ <- H.modify $ \state ->
            state { focus = coords }
        pure unit
