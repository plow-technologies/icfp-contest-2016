module Origami.Folding where




import Diagrams.Prelude hiding (Segment)
import Diagrams.TwoD.Vector

import Origami.Numbers
import Control.Lens
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Foldable (toList)


import Data.Set (Set)
import qualified Data.Set as Set

data Paper = Paper { vertices     :: Vertices
                  , facets       :: Set (Vector VertexIndex)}


-- Exterior vertex is one that is not inside any facet


data Segment a = Segment {  v0 :: a
                         , v1 :: a }
  deriving (Eq,Show,Ord)
type SegmentIdx = Segment Int
type SegmentVertex = Segment Vertex



-- Needs check
-- Create connected line segments
cycleNeighbors ::  Vector Vertex -> Vector SegmentVertex
cycleNeighbors vs = cycleIfLong  
          where
             cycleIfLong
               | Vector.length vs >= 2 = Vector.fromList . Vector.foldl' buildSegments buildOneSegment $ (Vector.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (vs!0) (vs!1)]

             
             buildSegments (seg:segs) v = (Segment (v0 seg) v ): seg : segs


cycleNeighborsIdx ::  Vector Int -> Vector SegmentIdx
cycleNeighborsIdx vs = cycleIfLong  
          where
             cycleIfLong
               | Vector.length vs >= 2 = Vector.fromList . Vector.foldl' buildSegments buildOneSegment $ (Vector.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (vs!0) (vs!1)]


             buildSegments (seg:segs) v = (Segment (v0 seg) v ): seg : segs

-- Make sure the point is in the polygon

testPointInside = (pointInside (V2 1.5 0.5) (Vector.fromList [(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]) == False) &&
                 (pointInside (V2 0.5 0.5) (Vector.fromList [(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]) == True) 


-- | Note a point that is onto the boundary is not inside it   
pointInside :: V2 Fraction -> Vector Vertex -> Bool
pointInside (V2 x y) vs = (Vector.length intersectSegments) `mod` 2 == 1
  where
     intersectSegments = Vector.filter (\p -> positiveXAxis p && aboveBelow p) allSegments
     allSegments = cycleNeighbors vs
     positiveXAxis (Segment (V2 x0 _ ) (V2 x1 _))  =  (x0 > x) || (x1 > x )

     aboveBelow (Segment (V2 _ y0) (V2 _ y1))   = ((y0 > y) && (y1 < y)) ||
                                                  ((y0 < y) && (y1 > y))




-- | Find all the vertices at the edge of our folds
exteriorVertices :: Paper -> Set Vertex
exteriorVertices paper = allExteriorVertices
  where
    vertexVector                                         = vertices paper
    facetSet                                             = facets paper
    allExteriorVertices                                  =  Vector.foldr checkVertexAgainstAllFacets Set.empty vertexVector            
    convertIndexToVertex vs                              = (\i -> vertexVector!i) <$> vs
    checkVertexAgainstAllFacets vertex exteriorVertexSet = if Set.member True (Set.map (pointInside vertex . convertIndexToVertex) facetSet)
                                                           then Set.insert vertex exteriorVertexSet
                                                           else exteriorVertexSet

-- | Find all creases that could be foldable
-- a foldable crease must have both edges as exterior vertices
outerCreases :: Paper -> Set (Segment Int)
outerCreases paper = exteriorFacetSegments
  where    
    exteriorVertices'              = exteriorVertices paper
    facetSet                       = facets paper
    exteriorFacetSegments          = Set.fold (\facet segments -> Set.union (Set.fromList . Vector.toList . findExteriorSegments $ facet) segments ) Set.empty facetSet
    findExteriorSegments facet     = Vector.filter (\(Segment vi0 vi1 ) -> (Bimap.member vi0 exteriorVertexBimap) ||
                                                                          (Bimap.member vi0 exteriorVertexBimap)  ) $ cycleNeighborsIdx facet
    exteriorVertexBimap            = Vector.foldr (\(i,v) map' ->
                                      if Set.member v exteriorVertices'
                                      then Bimap.insert i v map'
                                      else map'  ) Bimap.empty $ Vector.indexed . vertices $ paper


data ValidFold = ValidFold {  vertexIndex :: Int
                           ,targetIndex :: Vertex
                          , segment     :: Segment Int
                          } deriving (Eq, Show,Ord)


tryToFoldVertex :: Int -> Paper -> Set ValidFold
tryToFoldVertex i paper = Vector.foldr (\vdest valid -> Set.union valid $ Set.map (ValidFold i vdest) $ checkAll outerCreases' (vdest - vertex))
                                      Set.empty vertexVector
  where
    exteriorVertices'       = exteriorVertices paper    
    vertex                  = vertexVector!i
    vertexVector            = vertices paper
    outerCreases'           = outerCreases paper
    check v (Segment i0 i1)  = (v * ((vertexVector!i1) - (vertexVector!i0) ) == 0) &&
                                     isExteriorVertex

    isExteriorVertex            = Vector.foldr (\(i,v) map' ->
                                         if Set.member v exteriorVertices'
                                         then True
                                         else False  ) False $ Vector.indexed . vertices $ paper
    checkAll segmentSet v   = Set.filter (check v) segmentSet



type SourcePaper = Paper
type SinkPaper   = Paper

type Vertex = V2 Fraction

-- Tired of picking
type Vertices = (Vector Vertex)

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





