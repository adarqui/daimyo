{-# LANGUAGE NoMonomorphismRestriction #-}

module Daimyo.Graphs.Circles (
) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main = mainWith (circle 1 :: Diagram B R2)
