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


data Segment = Segment {  v0 :: Vertex
                       , v1 :: Vertex }


-- Needs check
-- Create connected line segments
cycleNeighbors :: Vector Vertex -> Vector Origami.Folding.Segment
cycleNeighbors vs = cycleIfLong  
          where
             cycleIfLong
               | Vector.length vs >= 2 = Vector.fromList . Vector.foldl' buildSegments buildOneSegment $ (Vector.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (vs!0) (vs!1)]

             
             buildSegments (seg:segs) v = (Segment (v0 seg) v ): seg : segs


-- Make sure the point is in the polygon
testPointInside = (pointInside (V2 1.5 0.5) (Vector.fromList [(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]) == False) &&
                 (pointInside (V2 0.5 0.5) (Vector.fromList [(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]) == True) 


pointInside :: V2 Fraction -> Vector Vertex -> Bool
pointInside (V2 x y) vs = (Vector.length intersectSegments) `mod` 2 == 1
  where
     intersectSegments = Vector.filter (\p -> positiveXAxis p && aboveBelow p) allSegments
     allSegments = cycleNeighbors vs
     positiveXAxis (Segment (V2 x0 _ ) (V2 x1 _))  =  (x0 > x) || (x1 > x )

     aboveBelow (Segment (V2 _ y0) (V2 _ y1))   = ((y0 > y) && (y1 < y)) ||
                                                     ((y0 < y) && (y1 > y))
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





-- foldPaper :: SourcePaper -> VertexToMove -> VertexToTarget -> SinkPaper
-- foldPaper source vM vT = _
--      where
--        foldDirection    = makeVector vM vT 
--        foldCrease       = orthVector foldDirection
--        validNewPaper    = findCrease source foldCrease



makeVector vM vT =  vT - vM



orthVector   = perp

findCrease  foldDirection   = Set.foldr (\a -> foldDirection)

sourceToSink = _

