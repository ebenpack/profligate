module Profligate.TreeViz where

import Data.Array as Arr
import Data.Char (toCharCode)
import Data.Foldable (foldr, sum)
import Data.Functor (map)
import Data.List (List(..), (:), reverse, null, snoc)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (class Show, class Eq, Unit, Void, bind, const, mod, not, pure, show, unit, (||), (&&), ($), (+), (*), (-), (/), (<>), (>), (==))
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Forest, Tree(..), treeMap)
import Profligate.Types (AnalysisMode(..))
import Profligate.Util (svg, rect, largeColorSet)

type Input = AnalysisMode

type Slot p = forall q. H.Slot q Void p

type ChildSlots = ()

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
    , analysisMode :: AnalysisMode
    }

data Query a =
    NoOpQ a

data Action =
      NoOp
    | Collapse TreeCoords
    | Uncollapse TreeCoords
    | Focus TreeCoords
    | SetAnalysisMode AnalysisMode

type AnnotatedCostCenterStackCosts =
    { depth :: Int
    , index :: Int
    , collapsed :: Boolean
    , stack :: CostCenterStackCosts
    , coords :: Maybe BisectoidCoords
    }

type AnnotatedCostCenterStackTree = Forest AnnotatedCostCenterStackCosts

treeViz :: forall f o m. MonadEffect m => MonadAff m => Profile -> AnalysisMode -> H.Component HH.HTML f Input o m
treeViz { costCenterStack } analysisMode = H.mkComponent
    { initialState: const (initialState analysisMode)
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = eval
        , receive = \am -> Just $ SetAnalysisMode am
        }
    }
    where

    totalWidth :: Number
    totalWidth = 1000.0

    totalHeight :: Number
    totalHeight = 1000.0

    totalArea :: Number
    totalArea = totalWidth * totalHeight

    annotatedTree :: AnalysisMode -> Int -> BisectoidCoords -> Forest CostCenterStackCosts -> AnnotatedCostCenterStackTree
    annotatedTree am dep coords cs = reverse $ foldr (annotateTree am dep coords) Nil $ reverse cs

    annotateTree :: AnalysisMode -> Int -> BisectoidCoords -> Tree CostCenterStackCosts -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    annotateTree am depth coords (Node { value, children }) Nil =
        let { width, height, direction } = getNewCoords am coords value
            newCoords = { x: coords.x, y: coords.y, width, height, direction } in
        (Node
            { value: { index: 0, depth, collapsed: true, stack: value, coords: Just newCoords }
            , children: (annotatedTree am (depth + 1) newCoords children) }
        ) : Nil
    annotateTree am depth outercoords (Node { value, children }) (n@(Node{ value: { index, stack, coords } }) : ns) =
        let justCoords = fromMaybe { x: 0.0, y: 0.0, width: 0.0, height: 0.0, direction: Horiz } coords
            horizontal = justCoords.direction == Horiz
            newX = if horizontal then justCoords.x else justCoords.x + justCoords.width
            newY = if horizontal then justCoords.y + justCoords.height else justCoords.y
            newWidth = outercoords.width - (newX - outercoords.x)
            newHeight = outercoords.height - (newY - outercoords.y)
            newCoords = getNewCoords am { x: newX, y: newY, width: newWidth, height: newHeight, direction: justCoords.direction } value in
        (Node
            { value: { index: index + 1, depth, collapsed: true, stack: value, coords: Just newCoords }
            , children: (annotatedTree am (depth + 1) newCoords children) }
        ) : n : ns

    getNewCoords :: AnalysisMode -> BisectoidCoords -> CostCenterStackCosts -> BisectoidCoords
    getNewCoords am coords@{ x, y, width, height, direction } cs =
        newCoords
        where
        newCoords = { x, y, width: newWidth, height: newHeight, direction: newDirection }
        area = ((getCost am cs) * 0.01) * totalArea
        newWidth =
            let newW = if horizontal || height == 0.0  then width else (area / height) in -- TODO: this probably shouldn't be zero... why is?
            if newW > 0.0 then newW else 0.0
        newHeight =
            let newH = if not horizontal || width == 0.0 then height else (area / width) in -- TODO: this probably shouldn't be zero... why is?
            if newH > 0.0 then newH else 0.0
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

    getCost :: AnalysisMode -> CostCenterStackCosts -> Number
    getCost am cs = case am of
        Time -> cs.inherited.time
        Alloc -> cs.inherited.alloc

    uncollapse :: TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    uncollapse coords stack = setin setUncollapsed coords stack
        where
        setUncollapsed (Node{ value, children }) = (Node{ value: value { collapsed = false }, children })

    collapse :: TreeCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    collapse coords stack = setin setCollapse coords stack
        where
        setCollapse :: Tree AnnotatedCostCenterStackCosts -> Tree AnnotatedCostCenterStackCosts
        setCollapse (Node{ value, children} ) = Node{ value: value { collapsed = true }, children: treeMap doCollapse children }
        doCollapse :: AnnotatedCostCenterStackCosts -> AnnotatedCostCenterStackCosts
        doCollapse s = s { collapsed = true }

    startingTree :: AnalysisMode -> AnnotatedCostCenterStackTree
    startingTree am = annotatedTree am 0 { x: 0.0, y: 0.0, width: totalWidth, height: totalHeight, direction: Horiz } costCenterStack

    initialState :: AnalysisMode -> State
    initialState am =
        { costCenterStack: (startingTree am)
        , focus: (0: Nil)
        , bisectoidViewWidth: 0
        , bisectoidViewHeight: 0
        , analysisMode: am
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
        focusRing :: Array (H.ComponentHTML Action ChildSlots m)
        focusRing = fromMaybe [] $ do
            cs' <- getin focus cs
            { x, y, width, height } <- cs'.coords
            pure
                [ rect
                    [ HP.attr (H.AttrName "x") (show (x + 1.5)) -- TODO: precalculate shared stuff
                    , HP.attr (H.AttrName "y") (show (y + 1.5))
                    , HP.attr (H.AttrName "width") (show (width - 3.0))
                    , HP.attr (H.AttrName "height") (show (height - 3.0))
                    , HP.attr (H.AttrName "stroke") "black"
                    , HP.attr (H.AttrName "stroke-width") "3"
                    , HP.attr (H.AttrName "stroke-dasharray") "5,5,5"
                    , HP.attr (H.AttrName "fill") "none"
                    ] []
                , rect
                    [ HP.attr (H.AttrName "x") (show (x + 1.5))
                    , HP.attr (H.AttrName "y") (show (y + 1.5))
                    , HP.attr (H.AttrName "width") (show (width - 3.0))
                    , HP.attr (H.AttrName "height") (show (height - 3.0))
                    , HP.attr (H.AttrName "stroke") "white"
                    , HP.attr (H.AttrName "stroke-width") "3"
                    , HP.attr (H.AttrName "stroke-dasharray") "0,5,0"
                    , HP.attr (H.AttrName "fill") "none"
                    ] []
                ]

    showBisectoidHelper :: TreeCoords -> TreeCoords -> AnnotatedCostCenterStackTree -> Array (H.ComponentHTML Action ChildSlots m)
    showBisectoidHelper focus treeCoords cs = row
        where
            row :: Array (H.ComponentHTML Action ChildSlots m)
            row = (Arr.foldr rowHelper [] $ Arr.fromFoldable cs)

            rowHelper :: Tree AnnotatedCostCenterStackCosts -> Array (H.ComponentHTML Action ChildSlots m) -> Array (H.ComponentHTML Action ChildSlots m)
            rowHelper c@(Node{ value: { index, stack: { inherited: { time } } } }) acc = (drawBisectoidView focus (snoc treeCoords index) c) <> acc

    drawBisectoidView :: TreeCoords -> TreeCoords -> Tree AnnotatedCostCenterStackCosts -> Array (H.ComponentHTML Action ChildSlots m)
    drawBisectoidView focus currentCoords (Node{ value: { stack: cs@{ name }, collapsed, depth, index, coords }, children }) =
        drawBox <> rest
        where
        rest =
            if collapsed
            then []
            else showBisectoidHelper focus currentCoords children
        drawBox =
            case coords of
                Just { x, y, width, height } ->
                    [ rect
                        [ HE.onClick (\_ -> Just (Focus currentCoords))
                        , HP.attr (H.AttrName "x") (show x)
                        , HP.attr (H.AttrName "y") (show y)
                        , HP.attr (H.AttrName "width") (show width)
                        , HP.attr (H.AttrName "height") (show height)
                        , HP.attr (H.AttrName "fill") (getColor cs) -- TODO: Fix coloring
                        , HP.attr (H.AttrName "data-thing") name
                        ]
                        []
                    ]
                Nothing -> []

    -- Re-annotate the tree when switching between analysis modes... this duplicates some logic from the initial annotation,
    -- which sucks, so I should maybe re-think my approach at some point
    reannotateTree :: AnalysisMode -> BisectoidCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    reannotateTree am coords cs =
        reverse $ foldr (reannotator coords) Nil $ reverse cs
        where
        reannotator :: BisectoidCoords -> Tree AnnotatedCostCenterStackCosts -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
        reannotator innercoords (Node { value: v@{ stack }, children }) Nil =
            let { width, height, direction } = getNewCoords am innercoords stack
                newCoords = { x: innercoords.x, y: innercoords.y, width, height, direction } in
            (Node
                { value: v { coords = Just newCoords }
                , children: (reannotateTree am newCoords children) }
            ) : Nil
        reannotator outercoords (Node { value: v@{ stack }, children }) (n@(Node{ value: { coords: innercoords } }) : ns) =
            let justCoords = fromMaybe { x: 0.0, y: 0.0, width: 0.0, height: 0.0, direction: Horiz } innercoords
                horizontal = justCoords.direction == Horiz
                newX = if horizontal then justCoords.x else justCoords.x + justCoords.width
                newY = if horizontal then justCoords.y + justCoords.height else justCoords.y
                newWidth = outercoords.width - (newX - outercoords.x)
                newHeight = outercoords.height - (newY - outercoords.y)
                newCoords = getNewCoords am { x: newX, y: newY, width: newWidth, height: newHeight, direction: justCoords.direction } stack in
            (Node
                { value: v { coords = Just newCoords }
                , children: (reannotateTree am newCoords children) }
            ) : n : ns


    eval :: Action -> H.HalogenM State Action ChildSlots o m Unit
    eval NoOp = pure unit
    eval (SetAnalysisMode mode) = do
        cs <- H.gets (\state -> state.costCenterStack)
        _ <- H.modify $ \state ->
            state
                { analysisMode = mode
                , costCenterStack = reannotateTree mode { x: 0.0, y: 0.0, width: totalWidth, height: totalHeight, direction: Horiz } cs
                }
            -- TODO: Analysis mode switching doesn't change the view, because the layout work is currently baked into the annotated tree
            -- and switching modes doesn't touch the tree... ideally the layout work would move into the render function
        pure unit
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

-- Get a pretty dumb hash of the module and use that to select a color
-- Collisions are likely
-- TODO: Move to util?
getColor :: CostCenterStackCosts -> String
getColor cs =
    let tot = sum (map toCharCode (toCharArray (cs.mod <> cs.name <> cs.src)))
        idx = tot `mod` (Arr.length largeColorSet)
    in case (Arr.(!!) largeColorSet idx) of
        Just col' -> col'
        _ -> "red"
