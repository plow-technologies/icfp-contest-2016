module Origami.Paper where

import Origami.Numbers
import Origami.Solution
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr())
import Data.Maybe (mapMaybe)

-- | A crease is the label for an edge
data Crease
  = Crease
  { _start :: !Coordinate
  , _end :: !Coordinate
  , _creasePaperToFolded :: !HomogenousMatrix
  , _creaseFoldedToPaper :: !HomogenousMatrix
  }

-- | A graph node is the label for a node. There should only be one node (0) labeled 'Outside'
data GraphNode
  = Outside
  | Facet
    { _facetPaperToFolded :: !HomogenousMatrix
    , _facetFoldedToPAper :: !HomogenousMatrix
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
  [ (0,1,Crease (Coordinate 0 0) (Coordinate 1 0) identityMatrix identityMatrix)
  , (0,2,Crease (Coordinate 1 0) (Coordinate 1 1) identityMatrix identityMatrix)
  , (0,3,Crease (Coordinate 1 1) (Coordinate 0 1) identityMatrix identityMatrix)
  , (0,4,Crease (Coordinate 0 1) (Coordinate 0 0) identityMatrix identityMatrix)
  ]

-- | All the ways of creasing the paper along the given line
foldsOnLine :: Paper -> Line -> [Paper]
foldsOnLine paper line = 
  let
    -- The edges in their folded positions
    edges = map _asFolded $ labEdges paper -- All the edges in their folded positions

    -- The edges that intersect the given line, together with the intersection point
    intersectingEdges =
       flip mapMaybe edges $ \edge@(_, _, Crease start end _ _) -> do
         intersection <- lineSegmentIntersection line start end
         return (edge, intersection)

    -- Chains of edges such that they share a node (i.e. the crease crosses the node)
    chainEdges = _

    -- The updated graphs, with
    --  * Crossed edges deleted and replaced by their splits
    --  * Crossed nodes deleted and replaced by their splits, distributing remaining edges appropriately
    --  ---- at this point the graph is disconnected. We apply the reflection to each subgraph individually, then join
    --  ---- reflected and undirected subgraph by
    --  * Edges across split nodes signifying crease lines
    --  REMEMBER TO APPLY THE PAPER->FOLD MATRIX TO ALL NEW EDGES IN THE GRAPH. COMPUTATIONS ARE DONE IN FOLDSPACE.
  in _
  where
    nodeEdges :: Node -> [(LEdge Crease, Coordinate)] -> [(LEdge Crease, Coordinate)]
    nodeEdges node = filter (\((start, end, crease), coordinate) -> start == node || end == node)

-- | Get the folded shape of the paper as polygons and holes
foldedShape :: Paper -> [[Coordinate]]
foldedShape paper = _

-- | Serialize the graph as a solution
paperSolution :: Paper -> Solution
paperSolution paper = _


