module Prof where

import Prelude

import Data.DateTime (DateTime)
import Data.List (List)
import Data.Maybe (Maybe)

newtype Tree a = Node { value :: a
                      , children :: Forest a 
                      }

type Forest a = List (Tree a)

derive instance eqTree :: (Eq a) => Eq (Tree a)

instance showTree :: (Show a) => Show (Tree a) where
  show (Node { value: a, children: t }) = "Node " <> show a <> show t

type TotalTime =
    { time :: Number
    , ticks :: Int
    , interval :: Int
    , processors :: Int
    }

type CostCenter l =
    { name :: String
    , mod :: String
    , src :: String
    , ticks :: Maybe Int
    , bytes :: Maybe Int
    | l
    }

type PerCostCenterCosts = CostCenter (time :: Number , alloc :: Number)

type CostCenterStackCosts = CostCenter
    ( number :: Int
    , entries :: Int
    , individual :: { time :: Number , alloc :: Number }
    , inherited :: { time :: Number , alloc :: Number }
    )

type Profile =
    { timestamp :: DateTime
    , title     :: String
    , totalTime :: TotalTime
    , totalAlloc :: Int
    , perCostCenterCosts :: List PerCostCenterCosts
    , costCenterStack :: Forest CostCenterStackCosts
    }