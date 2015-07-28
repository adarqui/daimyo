module Daimyo.Algorithm.Network.Pipeline (
  Channel,
  PipeProcess,
  PipeProcessList,
  pipe
) where

type Channel a         = [a]
type PipeProcess a     = Channel a -> Channel a
type PipeProcessList a = [PipeProcess a]

-- | pipe
--
-- taken from AAFA
--
pipe :: PipeProcessList a -> Channel a -> Channel a
pipe ps input = foldr (.) id ps $ input
