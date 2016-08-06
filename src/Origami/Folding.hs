{-# LANGUAGE OverloadedLists #-}
module Origami.Folding where




import Diagrams.Prelude hiding (Segment)
import Diagrams.TwoD.Vector

import Origami.Numbers
import Control.Lens
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap


import qualified Data.Sequence as Seq
import Data.Sequence ((<|),(|>),(><), Seq)

import qualified Data.Vector as Vector
import Data.Foldable (toList,foldl')


import Data.Set (Set)
import qualified Data.Set as Set





{--



1. to do a fold you have to have a crease orthogonal to the fold direction
2. folding from big to little seems like a good strategy.  I.E. Make big folds first!


Rules for a crease:

Crease must be bound on current outer polygon edge (point on the boundary).
Crease must be orthogonal to the direction of the fold

These two rules constrain the process of fold selection.

The idea here is you have your initial Paper.

Then use findValidFoldsForVertex on every exterior point. (preferably simultaneously)

This gives you a set of folds you could make.  So based on some rule, (I say the biggest fold)
you choose a fold to do.  This involves recalculating all the vertices on the initial side of the
crease to spots on the final side of the crease. (TO BE IMPLEMENTED).

Then checking to see A. are we the winner! B. are there any more valid folds left to do.

If the answer to B is no, we need to go back and get another point.

Assuming we have a valid skeleton this method should produce a right answer.  Though it is slow!




--}





-- foldPaper :: SourcePaper -> VertexToMove -> VertexToTarget -> SinkPaper
-- foldPaper source vM vT = _
--      where
--        foldDirection    = makeVector vM vT 
--        foldCrease       = orthVector foldDirection
--        validNewPaper    = findCrease source foldCrease










data Paper = Paper { vertices     :: Vertices
                  , facets       :: Seq (Seq VertexIndex)} -- ordered sequence of ordered vertex
  deriving (Eq,Ord,Show)

-- Exterior vertex is one that is not inside any facet


data Segment a = Segment {  v0 :: a
                         , v1 :: a }
  deriving (Eq,Show,Ord)
type SegmentIdx = Segment Int
type SegmentVertex = Segment Vertex




type SourcePaper = Paper
type SinkPaper   = Paper

type Vertex = V2 Fraction

type Vertices = [Vertex]

type VertexIndex = Int

type EdgeVertex    =  V2 Fraction 



type VertexToTarget = V2 Fraction
type VertexToMove   = V2 Fraction



-- Needs check
-- Create connected line segments
cycleNeighbors ::  Seq Vertex -> Seq SegmentVertex
cycleNeighbors vs = cycleIfLong  
          where
             cycleIfLong
               | Seq.length vs >= 2 = Seq.fromList . reverse . foldl' buildSegments buildOneSegment $ (Seq.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (Seq.index vs 0) (Seq.index vs 1)]             
             buildSegments (seg:segs) v = (Segment (v1 seg) v ): seg : segs  


cycleNeighborsIdx ::  Seq Int -> Seq SegmentIdx
cycleNeighborsIdx vs = cycleIfLong  
          where
             cycleIfLong
               | Seq.length vs >= 2 = Seq.fromList . reverse . foldl' buildSegments buildOneSegment $ (Seq.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (Seq.index vs 0) (Seq.index vs 1)]


             buildSegments (seg:segs) v = (Segment (v0 seg) v ): seg : segs








-- Make sure the point is in the polygon
testPointInside = (pointInside (V2 1.5 0.5) (Seq.fromList [(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]) == False) &&
                 (pointInside (V2 0.5 0.5) (Seq.fromList [(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]) == True) 


-- | Note a point that is onto the boundary is not inside it   
pointInside :: V2 Fraction -> Seq Vertex -> Bool
pointInside (V2 x y) vs = (Seq.length intersectSegments) `mod` 2 == 1
  where
     intersectSegments = Seq.filter (\p -> positiveXAxis p && aboveBelow p) allSegments
     allSegments = cycleNeighbors vs
     positiveXAxis (Segment (V2 x0 _ ) (V2 x1 _))  =  (x0 > x) || (x1 > x )

     aboveBelow (Segment (V2 _ y0) (V2 _ y1))   = ((y0 > y) && (y1 < y)) ||
                                                  ((y0 < y) && (y1 > y))


fromSeq = Set.fromList . toList

-- | Find all the vertices at the edge of our folds
exteriorVertices :: Paper -> Set Vertex
exteriorVertices paper = allExteriorVertices
  where
    vertexList                                           = vertices paper
    facetSet                                             = fromSeq . facets $ paper
    allExteriorVertices                                  =  foldr checkVertexAgainstAllFacets Set.empty vertexList            
    convertIndexToVertex vs                              = (\i -> vertexList!!i) <$> vs
    checkVertexAgainstAllFacets vertex exteriorVertexSet = if Set.member True (Set.map (pointInside vertex . convertIndexToVertex) facetSet)
                                                           then Set.insert vertex exteriorVertexSet
                                                           else exteriorVertexSet


-- | Find all segments whose vertices lay exclusively on the boundaries
-- exteriorSegments :: Paper -> Set Segments
-- exteriorSegments paper = 
--   where    
--     exteriorVertices'              = exteriorVertices paper
--     facetSet                       = facets paper
    


-- | Find all creases that could be foldable
-- a foldable crease must have both edges as exterior vertices
outerCreases :: Paper -> Set (Segment Int)
outerCreases paper = exteriorFacetSegments
  where    
    exteriorVertices'              = exteriorVertices paper
    facetSet                       = fromSeq $ facets paper
    exteriorFacetSegments          = Set.fold (\facet segments -> Set.union (Set.fromList . toList . findExteriorSegments $ facet) segments ) Set.empty facetSet
    findExteriorSegments facet     = Seq.filter (\(Segment vi0 vi1 ) -> (Bimap.member vi0 exteriorVertexBimap) ||
                                                                       (Bimap.member vi0 exteriorVertexBimap)  ) $ cycleNeighborsIdx facet
    exteriorVertexBimap            = foldr (\(i,v) map' ->
                                      if Set.member v exteriorVertices'
                                      then Bimap.insert i v map'
                                      else map'  ) Bimap.empty $ zip [0 .. ] ( vertices $ paper)


data ValidFold = ValidFold {  vertexIndex :: Int
                           ,targetIndex :: Vertex
                          , segment     :: Segment Int
                          } deriving (Eq, Show,Ord)



-- |Find the fold points that are valid for a given vertex index and paper
-- Valid folds have the location that is being folded to as well as the segment being folded over

findValidFoldsForVertex :: Int -> Paper -> Set ValidFold
findValidFoldsForVertex i paper = foldr (\vdest valid -> Set.union valid $ Set.map (ValidFold i vdest) $ checkAll outerCreases' (vdest - vertex))
                                                    Set.empty vertexList
  where
    exteriorVertices'       = exteriorVertices paper    
    vertex                  = vertexList!!i
    vertexList            = vertices paper
    outerCreases'           = outerCreases paper
    check v (Segment i0 i1)  = (v * ((vertexList!!i1) - (vertexList!!i0) ) == 0) &&
                                     isExteriorVertex

    isExteriorVertex            = foldr (\(i,v) map' ->
                                         if Set.member v exteriorVertices'
                                         then True
                                         else False  ) False $ zip [0 ..] . vertices $ paper
    checkAll segmentSet v   = Set.filter (check v) segmentSet







-- Fold something


testPaper = Paper [(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)] ([[0, 1 ,2 ,3]])

outputPaperAfterTriFold = (foldPaper testPaper 3 2 ) == (Paper [(V2 0 0), (V2 0 1), (V2 1 1)] ([[0, 1 ,2 ]]))



-- Everything for the fold
foldPaper paper initialIndex finalIndex = _
  where
    initialVertex           = (vertices paper)!! initialIndex
    finalVertex             = (vertices paper)!! finalIndex
    exteriorVertices'       = exteriorVertices paper   -- initial index must be exterior
    creaseLine              = crease paper initialIndex finalIndex
    findCreaseLine
       | Set.member initialVertex exteriorVertices' = findExteriorIntersection
       | otherwise = error "non exterior vertex"
       
    findExteriorIntersection = _
    


-- Return the new vertices created by the crease.
-- no check on exterior!
crease :: Paper -> Int -> Int -> LineC
crease paper initialIndex finalIndex  = creaseLine
  where
    initialVertex           = (vertices paper)!! initialIndex
    finalVertex             = (vertices paper)!! finalIndex
    exteriorVertices'       = exteriorVertices paper        -- initial index must be exterior    
    directionVertex         = finalVertex - initialVertex
    creasePoint             = -1 * directionVertex /0.5 + finalVertex
    creaseDirection         = perp directionVertex
    creaseLine              = lineEquation (creaseDirection + creasePoint) creasePoint
    
    



segmentIntersection  :: Segment (V2 Fraction) -> LineC -> Maybe (V2 Fraction)
segmentIntersection (Segment v1 v2) line = intersectionBetween v1 v2 line


-- | Simple Line Equation holder... 
data LineC = LineC {  lineM :: {-# UNPACK  #-} !Fraction,
                     lineB :: {-# UNPACK  #-}!Fraction}



-- | Generate a line equation from two Vertices
lineEquation :: V2 Fraction -> V2 Fraction -> LineC
lineEquation (V2 x1 y1) (V2 x2 y2) = LineC m b
  where
    m = (y1 - y2) / (x1 - x2)
    b = (y2*x1 - y1*x2) / (x1 - x2)



-- | Find the intersection point of two lines
intersection :: LineC -> LineC -> V2 Fraction
intersection (LineC m1 b1) (LineC m2 b2) = (V2 x y)
  where
    x = (b1 - b2) / (m1 - m2)
    y = (b2*m1 - b1*m2) / (m1 -m2 )





-- | check the line formed by two verticies against a line equation
-- to see if the intersection of the two lines falls between said vertices

intersectionBetween  :: V2 Fraction -> V2 Fraction -> LineC -> Maybe (V2 Fraction)
intersectionBetween v1 v2 linetest = between
    where
      vi = intersection (lineEquation v1 v2) linetest
      parallelDirections = dot (vi - v1) (vi - v2) > 0 -- parallel
      between =  if parallelDirections
                    then Nothing    -- parallel direction vectors mean a point not inbetween
                    else (Just vi) 


