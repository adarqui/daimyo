{-# LANGUAGE DeriveGeneric #-}

module Daimyo.Network.IPGen (
--  Ipv4Address (..),
  Ipv4Address,
  Ipv4Network (..),
  ipv4gen
) where

import GHC.Generics

{-
data Ipv4Address
  = Ipv4Address !Int !Int !Int !Int
  deriving (Show, Eq, Ord, Generic)
  -}
type Ipv4Address = Int

data Ipv4Network = Ipv4Network {
  addr :: Ipv4Address,
  mask :: Int,
  wildcard :: Int,
  broadcast :: Int,
  hostMin :: Int,
  hostMax :: Int,
  hosts :: Int
} deriving (Show, Eq, Ord, Generic)

ipv4gen = undefined
