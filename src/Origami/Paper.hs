module Origami.Paper where

import Origami.Numbers
import Origami.Solution
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr())

-- | An endpoint is a point on the paper together with a homogenous matrix transforming it to its destination
-- in the fold
data Endpoint
  = Endpoint
  { endpointCoordinate :: !Coordinate
  , endpointPaperToFolded :: HomogenousMatrix
  , endpointFoldedToPaper :: HomogenousMatrix
  }

initialEndpoint :: Fraction -> Fraction -> Endpoint
initialEndpoint x y = Endpoint (Coordinate x y) identityMatrix identityMatrix

-- | A crease is the label for an edge
data Crease
  = Crease
  { _start :: !Endpoint
  , _end :: !Endpoint
  }

-- | A graph node is the label for a node. There should only be one node (0) labeled 'Outside'
data GraphNode
  = Outside
  | Facet
    { facetPaperToFolded :: HomogenousMatrix
    , facetFoldedToPaper :: HomogenousMatrix
    }

-- | A line as the coefficients to the line equation ax + by + c = 0
type Line = (Fraction, Fraction, Fraction)

-- | The graph describing the facets of the paper
type Paper = Gr GraphNode Crease

-- | The transformation matrix for a facet in the folded sheet
data HomogenousMatrix
  = HomogenousMatrix
  { xInX :: !Fraction -- 11
  , yInX :: !Fraction -- 12
  , oneInX :: !Fraction -- 13
  , xInY :: !Fraction -- 21
  , yInY :: !Fraction -- 22
  , oneInY :: !Fraction -- 23
  , xInOne :: !Fraction  -- 31
  , yInOne :: !Fraction  -- 32
  , oneInOne :: !Fraction -- 33
  }

-- | Create a homogenous matrix from a 2X2 transformation matrix (internal pairs are rows)
fromTransformation :: ((Fraction, Fraction), (Fraction, Fraction)) -> HomogenousMatrix
fromTransformation ((xInX, yInX), (xInY, yInY)) =
  identityMatrix
  { xInX = xInX
  , yInX = yInX
  , xInY = xInY
  , yInY = yInY
  }

-- | Create a homogenous matrix which translates by a given x and y
fromTranslation :: Coordinate -> HomogenousMatrix
fromTranslation (Coordinate x y) =
  identityMatrix
  { oneInX = x
  , oneInY = y
  }

-- | Compose two homogenous matrices, applying the right (second) argument
-- first, and then the left
after :: HomogenousMatrix -> HomogenousMatrix -> HomogenousMatrix
after a b =
  HomogenousMatrix
--         i1       1j       i2       2j       i3         3j
  { xInX = xInX a * xInX b + yInX a * xInY b + oneInX a * xInOne b -- i = 1, j = 1
  , yInX = xInY a * xInX b + yInY a * xInY b + oneInY a * xInOne b -- i = 2, j = 1
  , oneInX = xInOne a * xInX b + yInOne a * xInY b + oneInOne a * oneInX b -- i = 3, j = 1
  , xInY = xInX a * yInX b + yInX a * yInY b + oneInX a * yInOne b -- i = 1, j = 2
  , yInY = yInX a * xInY b + yInY a * yInY b + oneInY a * yInOne b -- i = 2, j = 2
  , oneInY = xInOne a * yInX b + yInOne a * yInY b + oneInOne a * oneInY b -- i = 3, j = 2
  , xInOne = xInX a * oneInX b + yInX a * oneInY b + oneInX a * oneInOne b -- i = 1, j = 3
  , yInOne = xInY a * oneInX b + yInY b * oneInY b + oneInY b * oneInOne b -- i = 2 , j = 3
  , oneInOne = xInOne a * oneInX b + yInOne a * oneInY b + oneInOne a * oneInOne b -- i = 3, j = 3
  }

apply :: HomogenousMatrix -> Coordinate -> Coordinate
apply matrix coordinate =
  Coordinate { _x = (xInX matrix * _x coordinate + yInX matrix * _y coordinate + oneInX matrix) / oneInOne matrix
             , _y = (xInY matrix * _x coordinate + yInY matrix * _y coordinate + oneInY matrix) / oneInOne matrix
             }

-- | The matrix which leaves points alone
identityMatrix :: HomogenousMatrix
identityMatrix =
  HomogenousMatrix
  { xInX = 1
  , yInX = 0
  , oneInX = 0
  , xInY = 0
  , yInY = 1
  , oneInY = 0
  , xInOne = 0
  , yInOne = 0
  , oneInOne = 1
  }

-- | The line touching the given coordinates as ax + by + c = 0
lineABC :: Coordinate -> Coordinate -> (Fraction, Fraction, Fraction)
lineABC start end =
  (_y start - _y end, _x start - _x end, _x start * _y end - _x end * _y start)

-- | The intersection of two lines in ax + by + c = 0 form, if they intersect
lineIntersection :: Line -> Line -> Maybe Coordinate
lineIntersection (a1, b1, c1) (a2, b2, c2) =
  let
    determinant = a1 * b2 - a2 * b1
  in if determinant == 0
     then Nothing
     else Just $ Coordinate ((b2 * c1 - b1 * c2) / determinant) ((a1 * c2 - a2 * c1) / determinant)

-- | The intersection of a line with a segment, if they intersect
lineSegmentIntersection :: Line -> Coordinate -> Coordinate -> Maybe Coordinate
lineSegmentIntersection line start end = do
  intersection <- lineIntersection line (lineABC start end)
  if between intersection start end
    then Just intersection
    else Nothing

-- | Dot two coordinates
dotProduct :: Coordinate -> Coordinate -> Fraction
dotProduct a b = _x a * _x b + _y a * _y b

-- | Subtract two coordinates
minusCoordinate :: Coordinate -> Coordinate -> Coordinate
minusCoordinate a b = Coordinate { _x = _x a - _x b, _y = _y a - _y b }

-- | Check if a coordinate is between two others
between :: Coordinate -> Coordinate -> Coordinate -> Bool
between coordinate start end = minusCoordinate coordinate start `dotProduct` minusCoordinate coordinate end < 0

-- | Given the definition of a line (a, b, c) where ax + by + c = 0, compute the reflection matrix across that line
abcToReflection :: (Fraction, Fraction, Fraction) -> HomogenousMatrix
abcToReflection (a, b, c) =
  let aSquared = a * a
      bSquared = b * b
      minus2AB = (-2) * a * b
      minus2C = (-2) * c
  in
    HomogenousMatrix
    { xInX = bSquared - aSquared
    , yInX = minus2AB
    , oneInX = minus2C * a
    , xInY = minus2AB
    , yInY = aSquared - bSquared
    , oneInY = minus2C * b
    , xInOne = 0
    , yInOne = 0
    , oneInOne = aSquared + bSquared
    }

-- | The initial paper
initialPaper :: Paper
initialPaper =
  mkGraph
  -- Initial nodes
  [ (0, Facet identityMatrix identityMatrix)
  , (1, Outside)
  , (2, Outside)
  , (3, Outside)
  , (4, Outside)
  ]
  -- Initial edges
  [ (0,1,Crease (initialEndpoint 0 0) (initialEndpoint 1 0))
  , (0,2,Crease (initialEndpoint 1 0) (initialEndpoint 1 1))
  , (0,3,Crease (initialEndpoint 1 1) (initialEndpoint 0 1))
  , (0,4,Crease (initialEndpoint 0 1) (initialEndpoint 0 0))
  ]

-- | Specify a crease of a facet by picking two edges and two points (by proportion of the edges from start)
-- on the edges
crease :: (LEdge Crease, Fraction)
       -> (LEdge Crease, Fraction)
       -> Paper
       -> Maybe Paper -- ^ 'Nothing' if e.g. the edges don't share a facet or the fractions aren't in (0,1)
crease (edge1, proportion1) (edge2, proportion2) paper = _

-- | Find all the facets which are under the given line when folded
facetsOnLine :: (Fraction, Fraction, Fraction) -> [LNode GraphNode]
facetsOnLine line = _

-- | Get the folded shape of the paper as polygons and holes
foldedShape :: Paper -> [[Coordinate]]
foldedShape paper = _

-- | Serialize the graph as a solution
paperSolution :: Paper -> Solution
paperSolution paper = _


