module Profligate.TreeViz where

import Data.Array as Arr
import Data.Char (toCharCode)
import Data.Foldable (foldr)
import Data.List (List(..), (:), reverse, null, snoc)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (class Show, class Eq, Unit, Void, bind, const, mod, not, pure, show, unit, (||), (&&), ($), (+), (*), (-), (/), (<>), (>), (==))
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Forest, Tree(..))
import Profligate.Util (svg, rect)

type Slot = H.Slot Query Void

type TreeCoords = List Int

data BisectoidDirection = Vert | Horiz

instance showBisectoidDirection :: Show BisectoidDirection where
    show Vert = "Vert"
    show Horiz = "Horiz"

instance eqBisectoidDirection :: Eq BisectoidDirection where
    eq Vert Vert = true
    eq Horiz Horiz = true
    eq _ _ = false

type BisectoidCoords =
    { x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    , direction :: BisectoidDirection
    }

type State =
    { costCenterStack :: AnnotatedCostCenterStackTree
    , focus :: TreeCoords
    , bisectoidViewWidth :: Int
    , bisectoidViewHeight :: Int
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
    , coords :: BisectoidCoords
    }

type AnnotatedCostCenterStackTree = Forest AnnotatedCostCenterStackCosts

treeViz :: forall f i o m. MonadEffect m => MonadAff m => Profile -> H.Component HH.HTML f i o m
treeViz { costCenterStack } = H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = eval }
    }
    where

    totalWidth :: Number
    totalWidth = 1000.0

    totalHeight :: Number
    totalHeight = 1000.0

    totalArea :: Number
    totalArea = totalWidth * totalHeight

    annotatedTree :: Int -> BisectoidCoords -> Forest CostCenterStackCosts -> AnnotatedCostCenterStackTree
    annotatedTree dep coords cs = reverse $ foldr (annotateTree dep coords) Nil $ reverse cs

    annotateTree :: Int -> BisectoidCoords -> Tree CostCenterStackCosts -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    annotateTree depth coords (Node { value, children }) Nil =
        let { width, height, direction } = getNewCoords coords value
            newCoords = { x: coords.x, y: coords.y, width, height, direction } in
        (Node
            { value: { index: 0, depth, collapsed: true, stack: value, coords: newCoords }
            , children: (annotatedTree (depth + 1) newCoords children) }
        ) : Nil
    annotateTree depth outercoords (Node { value, children }) (n@(Node{ value: { index, stack, coords } }) : ns) =
        let horizontal = coords.direction == Horiz
            newX = if horizontal then coords.x else coords.x + coords.width
            newY = if horizontal then coords.y + coords.height else coords.y
            newWidth = outercoords.width - (newX - outercoords.x)
            newHeight = outercoords.height - (newY - outercoords.y)
            newCoords = getNewCoords { x: newX, y: newY, width: newWidth, height: newHeight, direction: coords.direction } value in
        (Node
            { value: { index: index + 1, depth, collapsed: true, stack: value, coords: newCoords }
            , children: (annotatedTree (depth + 1) newCoords children) }
        ) : n : ns

    getNewCoords :: BisectoidCoords -> CostCenterStackCosts -> BisectoidCoords
    getNewCoords coords@{ x, y, width, height, direction } cs =
        -- trace ((show newCoords) <> " " <> show cs ) \_ ->
        newCoords
        where
        newCoords = { x, y, width: newWidth, height: newHeight, direction: newDirection }
        area = (cs.inherited.time * 0.01) * totalArea
        newWidth = if horizontal || height == 0.0  then width else (area / height)
        newHeight = if not horizontal || width == 0.0 then height else (area / width)
        newDirection = if not (width == 0.0) && (area / width) > width then Horiz else Vert
        horizontal = newDirection == Horiz

    lapse :: Boolean -> TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    lapse b coords stack = setin setCollapsed coords stack
        where
        setCollapsed (Node{ value, children}) = (Node{ value: value { collapsed = b }, children })

    setin :: (Tree AnnotatedCostCenterStackCosts -> Tree AnnotatedCostCenterStackCosts) -> TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    setin f (0:Nil) (y:ys) = f y : ys
    setin f (0:ns) (Node{ value, children}:ys) = (Node{ value, children: setin f ns children }):ys
    setin f (m:ns) (y:ys) = y : setin f ((m-1):ns) ys
    setin _ _ xs = xs

    getin :: TreeCoords -> AnnotatedCostCenterStackTree -> Maybe AnnotatedCostCenterStackCosts
    getin (0:Nil) ((Node{ value }):ys) = Just value
    getin (0:ns) (Node{ value, children}:ys) = getin ns children
    getin (m:ns) (y:ys) = getin ((m-1):ns) ys
    getin _ xs = Nothing

    uncollapse :: TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    uncollapse = lapse false -- TODO: Collapse childrens

    collapse :: TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    collapse = lapse true

    startingTree :: AnnotatedCostCenterStackTree
    startingTree = annotatedTree 0 { x: 0.0, y: 0.0, width: totalWidth, height: totalHeight, direction: Horiz } costCenterStack

    initialState :: State
    initialState =
        { costCenterStack: startingTree
        , focus: (0: Nil)
        , bisectoidViewWidth: 0
        , bisectoidViewHeight: 0
        }

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
                        [ HH.div
                            [ HP.attr (H.AttrName "class") "content" ]
                            [ showStats state.focus state.costCenterStack ]
                        ]
                    , HH.div
                        [ HP.attr (H.AttrName "class") "tree" ]
                        [ HH.div
                            [ HP.attr (H.AttrName "class") "content" ]
                            [ showTree state.focus state.costCenterStack ]
                        ]
                    ]
                ]
            , HH.div
                [ HP.attr (H.AttrName "class") "right" ]
                [ showBisectoid state.focus state.costCenterStack ]
            ]

    --------
    -- Stats
    --------

    showStats :: TreeCoords -> AnnotatedCostCenterStackTree -> H.ComponentHTML Action ChildSlots m
    showStats coords cs =
        case getin coords cs of
            Nothing -> HH.div [] []
            Just { stack } ->
                HH.div_
                    [ HH.div_ [ HH.h2_ [ HH.text stack.name ] ]
                    , HH.div_ [ HH.text $ "Module: " <> stack.mod ]
                    , HH.div_ [ HH.text $ "Entries: " <> show stack.entries ]
                    , HH.div_ [ HH.text $ "Time: " <> show stack.individual.time ]
                    , HH.div_ [ HH.text $ "Alloc: " <> show stack.individual.alloc ]
                    , HH.div_ [ HH.text $ "Time (inherited): " <> show stack.inherited.time ]
                    , HH.div_ [ HH.text $ "Alloc (inherited): " <> show stack.inherited.alloc ]
                    ]

    -------
    -- Tree
    -------

    showTree :: TreeCoords -> AnnotatedCostCenterStackTree -> H.ComponentHTML Action ChildSlots m
    showTree focus cs = HH.div [] (Arr.fromFoldable $ showTreeHelper focus Nil cs)

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
                [ drawTitle stack.name ] ] <> rest)
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
                            , HE.onClick (\_ -> Just (Uncollapse coords))
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
                    , HP.attr (H.AttrName "class") (if focus == coords then "name selected" else "name")
                    ]

    ------------
    -- Bisectoid
    ------------

    showBisectoid :: TreeCoords -> AnnotatedCostCenterStackTree -> H.ComponentHTML Action ChildSlots m
    showBisectoid focus cs =
        svg
            [ HP.attr (H.AttrName "viewBox") ("0 0 " <> (show totalWidth) <> " " <> (show totalHeight))
            , HP.attr (H.AttrName "width") (show totalWidth)
            , HP.attr (H.AttrName "height") (show totalHeight)
            , HP.attr (H.AttrName "style") "width: 100%; height: auto;"
            ]
            ((Arr.fromFoldable $ showBisectoidHelper focus Nil cs) <> focusRing)
        where
        focusRing = case getin focus cs of
            Just { coords: { x, y, width, height } } ->
                [ rect
                    [ HP.attr (H.AttrName "x") (show x)
                    , HP.attr (H.AttrName "y") (show y)
                    , HP.attr (H.AttrName "width") (show width)
                    , HP.attr (H.AttrName "height") (show height)
                    , HP.attr (H.AttrName "stroke") "black"
                    , HP.attr (H.AttrName "stroke-width") "2"
                    , HP.attr (H.AttrName "fill") "none"
                    ] [] ]
            Nothing -> []

    showBisectoidHelper :: TreeCoords -> TreeCoords -> AnnotatedCostCenterStackTree -> Array (H.ComponentHTML Action ChildSlots m)
    showBisectoidHelper focus treeCoords cs = row
        where
            row :: Array (H.ComponentHTML Action ChildSlots m)
            row = (Arr.foldr rowHelper [] $ Arr.fromFoldable cs)

            rowHelper :: Tree AnnotatedCostCenterStackCosts -> Array (H.ComponentHTML Action ChildSlots m) -> Array (H.ComponentHTML Action ChildSlots m)
            rowHelper c@(Node{ value: { index, stack: { inherited: { time } } } }) acc = (drawBisectoidView focus (snoc treeCoords index) c) <> acc

    drawBisectoidView :: TreeCoords -> TreeCoords -> Tree AnnotatedCostCenterStackCosts -> Array (H.ComponentHTML Action ChildSlots m)
    drawBisectoidView focus currentCoords (Node{ value: { stack: cs@{ name }, collapsed, depth, index, coords: { x, y, width, height } }, children }) =
        ([ drawBox ]) <> rest
        where
        rest =
            if collapsed
            then []
            else showBisectoidHelper focus currentCoords children
        drawBox =
            rect
                [ HP.attr (H.AttrName "x") (show x)
                , HP.attr (H.AttrName "y") (show y)
                , HP.attr (H.AttrName "width") (show width)
                , HP.attr (H.AttrName "height") (show height)
                , HP.attr (H.AttrName "fill") (getColor cs) -- TODO: Fix coloring
                , HP.attr (H.AttrName "data-thing") name
                ]
                []

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
    , "#FF0061"
    , "#4F57AA"
    , "#609000"
    , "#960084"
    , "#FDD456"
    , "#2FCE03"
    , "#ff7600"
    , "#00b8ff"
    ]

-- Get a pretty dumb hash of the module and use that to select a color
-- Collisions are likely
-- TODO: Move to util?
getColor :: CostCenterStackCosts -> String
getColor cs =
    let tot = Arr.foldr (\c acc -> acc + (toCharCode c)) 0 (toCharArray (cs.mod <> cs.name))
        idx = tot `mod` (Arr.length colors)
    in case (Arr.(!!) colors idx) of
        Just col' -> col'
        _ -> "red"
