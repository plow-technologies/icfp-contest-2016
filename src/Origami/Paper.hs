module Origami.Paper where

import Origami.Numbers
import Origami.Solution
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr())

-- | A crease is the label for an edge
data Crease
  = Crease
  { _start :: !Coordinate
  , _end :: !Coordinate
  }

-- | A graph node is the label for a node. There should only be one node (0) labeled 'Outside'
data GraphNode
  = Outside
  | Facet TransformationMatrix

-- | The graph describing the facets of the paper
type Paper = Gr GraphNode Crease

-- | The transformation matrix for a facet in the folded sheet
data TransformationMatrix

identityMatrix :: TransformationMatrix
identityMatrix = _

-- | The initial paper
initialPaper :: Paper
initialPaper =
  mkGraph
  -- Initial nodes
  [ (0, Outside)
  , (1, Facet identityMatrix)
  ]
  -- Initial edges
  [ (1,0,Crease (Coordinate 0 0) (Coordinate 1 0))
  , (1,0,Crease (Coordinate 1 0) (Coordinate 1 1))
  , (1,0,Crease (Coordinate 1 1) (Coordinate 0 1))
  , (1,0,Crease (Coordinate 0 1) (Coordinate 0 0))
  ]

-- | Specify a crease of a facet by picking two edges and two points (by proportion of the edges from start)
-- on the edges
crease :: (LEdge Crease, Fraction)
       -> (LEdge Crease, Fraction)
       -> Paper
       -> Maybe Paper -- ^ 'Nothing' if e.g. the edges don't share a facet or the fractions aren't in (0,1)
crease (edge1, proportion1) (edge2, proportion2) paper = _

-- | Find all the facets whose folded positions include the given coordinate.
facetsOnCoordinate :: Coordinate -> [LNode GraphNode]
facetsOnCoordinate coordinate = _

-- | Get the folded shape of the paper as polygons and holes
foldedShape :: Paper -> [[Coordinate]]
foldedShape paper = _

-- | Serialize the graph as a solution
paperSolution :: Paper -> Solution
paperSolution paper = _
