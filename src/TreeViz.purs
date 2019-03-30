module Profligate.TreeViz where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML (div)
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types as HVT
import Profligate.Profile.Profile (Profile, CostCenterStackCosts, Forest)

type Slot = H.Slot Query Void

type State =
    { foo :: String
    }

data Query a =
      NoOp a

data ChildQuery a = Foo a

type ChildState =
    { bar :: String
    }

-- treeViz :: forall m. MonadEffect m => MonadAff m => Profile -> H.ParentHTML Query ChildQuery Slot m
-- treeViz { costCenterStack } = H.component
--     { initialState: const initialState
--     , render
--     , eval
--     , receiver: const Nothing
--     }
--     where
--     initialState = { foo: "" }

--     render :: State -> H.ParentHTML Query ChildQuery Slot m
--     render state = 
--         HH.div
--         []
--         [ ]

--     eval :: Query ~> H.ComponentDSL State Query Void m
--     eval (NoOp next) = pure next

-- -- foobar :: forall m. MonadEffect m => MonadAff m => Forest CostCenterStackCosts -> H.ParentHTML ChildQuery ChildQuery Slot m
-- -- foobar cs = H.component
-- --     { initialState: const initialState
-- --     , render
-- --     , eval
-- --     , receiver: const Nothing
-- --     }
-- --     where
-- --     initialState = { bar: "" }

-- --     render :: ChildState -> H.ParentHTML ChildQuery ChildQuery Slot m
-- --     render state = HH.div 
-- --         [] 
-- --         [ HH.slot ButtonSlot (foobar cs) unit absurd
-- --         ]

-- --     eval :: ChildQuery ~> H.ParentDSL ChildState ChildQuery ChildQuery Slot Void m
-- --     eval (Foo next) = pure next