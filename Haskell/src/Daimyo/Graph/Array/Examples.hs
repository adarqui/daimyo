module Daimyo.Graph.Array.Examples (
  course_dependencies
) where

import           Daimyo.Graph.Array
import           GHC.Arr

data Courses
  = Math
  | Theory
  | Languages
  | Programming
  | Concurrency
  | Architecture
  | Parallelism
  deriving (Eq, Ord, Enum, Ix, Show)

course_dependencies :: [Courses]
course_dependencies = tsort $ mkGraph (Math, Parallelism)
  [
    (Math,Theory,1 :: Int),
    (Languages,Theory,1),
    (Programming,Languages,1),
    (Programming,Concurrency,1),
    (Concurrency,Parallelism,1),
    (Architecture,Parallelism,1)
  ]
