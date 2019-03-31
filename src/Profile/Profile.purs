module Profligate.Profile.Profile where

import Prelude

import Data.DateTime (DateTime)
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)

newtype Tree a = Node { value :: a
                      , children :: Forest a 
                      }

type Forest a = List (Tree a)

derive instance eqTree :: (Eq a) => Eq (Tree a)

instance showTree :: (Show a) => Show (Tree a) where
  show (Node { value: a, children: t }) = "Node " <> show a <> show t

depth :: forall a. Forest a -> Int
depth Nil = 0
depth xs = 1 + (foldr (\(Node { children: c}) acc -> max acc (depth c)) 0 xs)
    where
    max :: Int -> Int -> Int
    max a b = if a > b then a else b

filter :: forall a. (a -> Boolean) -> Forest a -> Forest a
filter _ Nil = Nil
filter f (n@Node { value, children }:xs) =
    if not $ f value
    then filter f xs
    else Node { value, children: filter f children } : filter f xs

treeMap :: forall a b. (a -> b) -> Forest a -> Forest b
treeMap _ Nil = Nil
treeMap f (Node { value, children }:xs) = (Node{ value: f value, children: treeMap f children }) : treeMap f xs

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