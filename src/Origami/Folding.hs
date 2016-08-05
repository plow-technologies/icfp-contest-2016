module Origami.Folding where




import Diagrams.Prelude
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Points
import Diagrams.Segment
import Origami.Silhouette
import Data.Vector (Vector,(!))
import Data.Foldable (toList)

import qualified Data.Vector as Vector

import Data.Set (Set)
import qualified Data.Set as Set

data Paper = Paper { vertices     :: Vector Vertex
                  , facets       :: Set (Vector VertexIndex)}


-- Exterior vertex is one that is not inside any facet

{--
  
-- | A point is inside a polygon if it has an odd number of intersections with the boundary (Jordan Curve theorem)
pointInside :: V2 -> Vector V2 -> Bool
pointInside (V x y) poly = (V.length intersectPairs) `mod` 2 == 1
  where intersectPairs = V.filter (\p -> positiveXAxis p && aboveBelow p) allPairs --, specialCases p]
        allPairs = cycleNeighbours poly
        positiveXAxis p = (x0 p) > x || (x1 p) > x -- intersect with positive x-axis
                                                   -- only lines with one point above + one point below can intersect
        aboveBelow p = (((y0 p)> y && (y1 p)< y) || ((y0 p) < y && (y1 p) > y))
        specialCases p = (((dir1 p) > 0 && (dir2 p) <= 0) || ((dir1 p) <= 0 && (dir2 p) > 0))-- cross product for special cases
        dir1 p = cross ((x1 p)-(x0 p),(y1 p)-(y0 p)) (1,0)
        dir2 p = cross ((x1 p)-(x0 p),(y1 p)-(y0 p)) (x-(x0 p),y-(y0 p))
        cross (a0,b0) (a1,b1) = a0*b1 - a1*b0
        x0 p = (\(V x y) -> x) (V.head p)
        x1 p = (\(V x y) -> x) (V.last p)
        y0 p = (\(V x y) -> y) (V.head p)
        y1 p = (\(V x y) -> y) (V.last p)
    --}


data Segment = Segment {  v0 :: Vertex
                       , v1 :: Vertex }


-- Needs check
cycleNeighbors vs = cycleIfLong  
          where
             cycleIfLong
               | Vector.length vs >= 2 = Vector.fromList . Vector.foldl' buildSegments buildOneSegment (Vector.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = Segment (vs!0) (vs!1)
             buildSegments v (seg:segs) = (Segment (v0 seg) v ): seg : segs




pointInside (V2 x y) vs = (Vector.length intersectPairs) `mod` 2 == 1
  where
     intersectPairs = Vector.filter (\p -> positiveXAxis p && aboveBelow p) allPairs
     
exteriorVertex (V2 x y) = _

hullFacets paper =   toList . toVertex. facets $ paper
  where
    vertexVector = vertices paper
    toVertex facetSet =  foldr fromVertexVector Set.empty facetSet
    fromVertexVector v set = Vector.foldr (\i set' -> Set.insert (p2 . unr2 $ vertexVector!i) set' ) set v


type SourcePaper = Paper
type SinkPaper   = Paper

type Vertex = V2 Fraction


type VertexIndex = Int

type EdgeVertex    =  V2 Fraction 



type VertexToTarget = V2 Fraction
type VertexToMove   = V2 Fraction


{--

Folds without holes

1. to do a fold you have to have a crease orthogonal to the fold direction
2. folding from big to little seems like a good strategy.  I.E. Make big folds first!


Rules for a crease:

Crease must be bound on current outer polygon edge (point on the hull boundary).
Crease must be orthogonal to the direction of the fold
--}





foldPaper :: SourcePaper -> VertexToMove -> VertexToTarget -> SinkPaper
foldPaper source vM vT = _
     where
       foldDirection    = makeVector vM vT 
       foldCrease       = orthVector foldDirection
       validNewPaper    = findCrease source foldCrease



makeVector vM vT =  vT - vM



orthVector   = perp

findCrease  foldDirection   = Set.foldr (\a -> foldDirection)

sourceToSink = _

