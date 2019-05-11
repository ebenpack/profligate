module Profligate.TreeViz where

import Prelude

import Data.Array as Arr
import Data.Char (toCharCode)
import Data.Foldable (foldr, sum)
import Data.Foldable as F
import Data.Function (on)
import Data.List (List(..), (:), reverse, null, snoc)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (isNaN)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (abs)
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Forest, Tree(..), treeMap)
import Profligate.Types (AnalysisMode(..))
import Profligate.Util (svg, rect, text, title, largeColorSet)

type Input = AnalysisMode

type Slot p = forall q. H.Slot q Void p

type ChildSlots = ()

type TreeCoords = List Int

type BisectoidCoords =
    { x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
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
    , coords :: BisectoidCoords
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

    annotatedTree :: Int -> Forest CostCenterStackCosts -> AnnotatedCostCenterStackTree
    annotatedTree dep cs = reverse $ foldr (annotateTree dep) Nil $ reverse cs

    annotateTree :: Int -> Tree CostCenterStackCosts -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    annotateTree depth (Node { value, children }) xs =
        (Node
            { value: { index: 0, depth, collapsed: true, stack: value, coords: { x: 0.0, y: 0.0, width: totalWidth, height: totalHeight } }
            , children: (annotatedTree (depth + 1) children) }
        ) : xs

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

    getCsCost :: AnalysisMode -> CostCenterStackCosts -> Number
    getCsCost am cs = case am of
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
    startingTree am =
        layoutTree am { x: 0.0, y: 0.0, width: totalWidth, height: totalHeight }
            $ annotatedTree 0 costCenterStack

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
                [ showBisectoid state.analysisMode state.focus state.costCenterStack ]
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

    showBisectoid :: AnalysisMode -> TreeCoords -> AnnotatedCostCenterStackTree -> H.ComponentHTML Action ChildSlots m
    showBisectoid am focus cs =
        svg
            [ HP.attr (H.AttrName "viewBox") ("0 0 " <> (show totalWidth) <> " " <> (show totalHeight))
            , HP.attr (H.AttrName "width") (show totalWidth)
            , HP.attr (H.AttrName "height") (show totalHeight)
            , HP.attr (H.AttrName "style") "width: 100%; height: auto;"
            ]
            ((Arr.fromFoldable $ showBisectoidHelper am focus Nil cs) <> focusRing)
        where
        focusRing :: Array (H.ComponentHTML Action ChildSlots m)
        focusRing = fromMaybe [] $ do
            cs' <- getin focus cs
            pure
                [ rect
                    [ HP.attr (H.AttrName "x") (show (cs'.coords.x + 1.5)) -- TODO: precalculate shared stuff
                    , HP.attr (H.AttrName "y") (show (cs'.coords.y + 1.5))
                    , HP.attr (H.AttrName "width") (show (abs (cs'.coords.width - 3.0)))
                    , HP.attr (H.AttrName "height") (show (abs (cs'.coords.height - 3.0)))
                    , HP.attr (H.AttrName "stroke") "black"
                    , HP.attr (H.AttrName "stroke-width") "3"
                    , HP.attr (H.AttrName "stroke-dasharray") "5,5,5"
                    , HP.attr (H.AttrName "fill") "none"
                    ] []
                , rect
                    [ HP.attr (H.AttrName "x") (show (cs'.coords.x + 1.5))
                    , HP.attr (H.AttrName "y") (show (cs'.coords.y + 1.5))
                    , HP.attr (H.AttrName "width") (show (abs (cs'.coords.width - 3.0)))
                    , HP.attr (H.AttrName "height") (show (abs (cs'.coords.height - 3.0)))
                    , HP.attr (H.AttrName "stroke") "white"
                    , HP.attr (H.AttrName "stroke-width") "3"
                    , HP.attr (H.AttrName "stroke-dasharray") "0,5,0"
                    , HP.attr (H.AttrName "fill") "none"
                    ] []
                ]

    showBisectoidHelper :: AnalysisMode -> TreeCoords -> TreeCoords -> AnnotatedCostCenterStackTree -> Array (H.ComponentHTML Action ChildSlots m)
    showBisectoidHelper am focus treeCoords cs = row
        where
            row :: Array (H.ComponentHTML Action ChildSlots m)
            row = (Arr.foldr rowHelper [] $ Arr.fromFoldable cs)

            rowHelper :: Tree AnnotatedCostCenterStackCosts -> Array (H.ComponentHTML Action ChildSlots m) -> Array (H.ComponentHTML Action ChildSlots m)
            rowHelper c@(Node{ value: { index, stack: { inherited: { time } } } }) acc = (drawBisectoidView am focus (snoc treeCoords index) c) <> acc

    -- TODO: Duplicated, move to util?
    displayStackInfo :: AnalysisMode -> CostCenterStackCosts -> String
    displayStackInfo am cs =
        "Function: " <> cs.name <>
            " (" <> show cs.entries <> " entries, " <> show (getCsCost am cs) <> "%)"

    drawBisectoidView :: AnalysisMode -> TreeCoords -> TreeCoords -> Tree AnnotatedCostCenterStackCosts -> Array (H.ComponentHTML Action ChildSlots m)
    drawBisectoidView am focus currentCoords (Node{ value: { stack: cs@{ name }, collapsed, depth, index, coords }, children }) =
        drawBox <> rest
        where
        rest =
            if collapsed
            then []
            else showBisectoidHelper am focus currentCoords children
        shouldNotDraw = coords.width == 0.0 || coords.height == 0.0 || isNaN coords.width || isNaN coords.width
        drawBox =
            if shouldNotDraw
            then []
            else
                [ rect
                    [ HE.onClick (\_ -> Just (Focus currentCoords))
                    , HP.attr (H.AttrName "x") (show coords.x)
                    , HP.attr (H.AttrName "y") (show coords.y)
                    , HP.attr (H.AttrName "width") (show coords.width)
                    , HP.attr (H.AttrName "height") (show coords.height)
                    , HP.attr (H.AttrName "fill") (getColor cs) -- TODO: Fix coloring
                    ]
                    [ title
                        []
                        [ text [] [ HH.text $ displayStackInfo am cs ]]
                    ]
                ]

    -- The tree needs to be re-laid out, then re-annotated when switching between analysis modes...
    layoutTree :: AnalysisMode -> BisectoidCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
    layoutTree am coords cs = 
        let treeified = treemap am 1.0 cs { x: 0.0, y: 0.0, width: totalWidth, height: totalHeight } in
        reannotate 0 treeified
        where
        addDepth :: Int -> Tree AnnotatedCostCenterStackCosts -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
        addDepth depth (Node{ value, children }) Nil =
            (Node
                { value: value { depth = depth, index = 0 }
                , children: (reannotate (depth + 1) children)
                }) : Nil
        addDepth depth (Node{ value, children }) (x@(Node{value:{index}}):xs) =
            (Node
                { value: value { depth = depth, index = index + 1 }
                , children: (reannotate (depth + 1) children)
                }) : x : xs
        reannotate :: Int -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree
        reannotate depth xs = reverse $ foldr (addDepth depth) Nil $ reverse xs

    eval :: Action -> H.HalogenM State Action ChildSlots o m Unit
    eval NoOp = pure unit
    eval (SetAnalysisMode mode) = do
        cs <- H.gets (\state -> state.costCenterStack)
        _ <- H.modify $ \state ->
            state
                { analysisMode = mode
                , costCenterStack = layoutTree mode { x: 0.0, y: 0.0, width: totalWidth, height: totalHeight } cs
                }
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
        idx = tot `mod` (Arr.length largeColorSet) in
    case (Arr.(!!) largeColorSet idx) of
        Just col -> col
        _ -> "red"

-- https://github.com/slamdata/purescript-treemap
treemap ∷ AnalysisMode -> Number -> AnnotatedCostCenterStackTree -> BisectoidCoords -> AnnotatedCostCenterStackTree
treemap am s xs coords@({ width, height }) =
  let totalCost = getTotalCost xs
      scale = if totalCost == 0.0 then 0.0 else (width * height / totalCost) * s
      squareParent = squarify getCost scale xs coords in
  map treemapChild squareParent
  where
  treemapChild c@(Node{ value, children }) =
    let childCost = getTotalCost children
        childScale = childCost / (getCost c) in
    Node{ value, children: treemap am childScale children value.coords }
  getCost (Node{ value: { stack: { name, inherited: { time, alloc }} } }) =
    case am of
        Time -> (time * 0.01)
        Alloc -> (alloc * 0.01)
  getTotalCost xs' = F.sum (getCost <$> xs')

squarify ∷ (Tree AnnotatedCostCenterStackCosts -> Number) -> Number -> AnnotatedCostCenterStackTree -> BisectoidCoords -> AnnotatedCostCenterStackTree
squarify f scale cs coords =
    let sorted = L.sortBy (flip compare `on` f) cs
    in join $ go Nil coords sorted Nil 0.0 (shortestSide coords)
    where
    go ∷ List (AnnotatedCostCenterStackTree) -> BisectoidCoords -> AnnotatedCostCenterStackTree -> AnnotatedCostCenterStackTree -> Number -> Number -> List (AnnotatedCostCenterStackTree)
    go result rect xs row s w =
        case xs of
            x : xs' ->
                let row' = x : row
                    s' = s + f x in
                if L.null row || worst row s w >= worst row' s' w
                then go result rect xs' row' s' w
                else
                    let rect' = trim (s * scale) rect
                        result' = layout row (s * scale) rect : result in
                    go result' rect' xs Nil 0.0 (shortestSide rect')
            _ -> layout row (s * scale) rect : result

    worst ∷ AnnotatedCostCenterStackTree -> Number -> Number -> Number
    worst row s w =
        let s2 = s * s
            w2 = w * w / scale
            rMin = maybe 0.0 f (L.head row)
            rMax = maybe 0.0 f (L.last row) in
        max (w2 * rMax / s2) (s2 / (w2 * rMin))

    layout ∷ AnnotatedCostCenterStackTree -> Number -> BisectoidCoords -> AnnotatedCostCenterStackTree
    layout row s { x, y, width, height } =
        if width >= height
        then 
            let aw = s / height in
            snd $ F.foldl
                (\(Tuple offset result) r@(Node{ value, children }) ->
                    let h = f r * scale / aw
                        newCoords = { x, y: offset, width: aw, height: h} in
                    Tuple (offset + h) ((Node{ value: value { coords = newCoords }, children }) : result))
                (Tuple y Nil)
                row
        else
            let ah = s / width in
            snd $ F.foldl
                (\(Tuple offset result) r@(Node{ value, children }) ->
                    let w = f r * scale / ah
                        newCoords = { x: offset, y, width: w, height: ah } in
                    Tuple (offset + w) ((Node{ value: value { coords = newCoords }, children }) : result))
                (Tuple x Nil)
                row

shortestSide ∷ BisectoidCoords -> Number
shortestSide { width, height } = min width height

trim ∷ Number -> BisectoidCoords -> BisectoidCoords
trim s { x, y, width, height } =
    if width >= height
    then let aw = s / height in { x: (x + aw), y, width: (width - aw), height }
    else let ah = s / width in { x, y: (y + ah), width, height: (height - ah) }
