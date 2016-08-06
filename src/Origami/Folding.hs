{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
module Origami.Folding where




import Diagrams.Prelude hiding (Segment)
import Diagrams.TwoD.Vector

import Origami.Numbers





import qualified Data.Sequence as Seq
import Data.Sequence (Seq)


import Data.Foldable (toList,foldl')

import Data.Maybe (catMaybes)


import Data.Set (Set) 
import Test.QuickCheck hiding (scale)
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


type SegmentIdx = Segment Int
type SegmentVertex = Segment Vertex




type SourcePaper = Paper
type SinkPaper   = Paper

type Vertex = V2 Fraction

type Vertices = Seq Vertex

type VertexIndex = Int

type EdgeVertex    =  V2 Fraction 



type VertexToTarget = V2 Fraction
type VertexToMove   = V2 Fraction

data Paper = Paper { _vertices     :: Vertices
                  , _facets       :: (Seq VertexIndex)}  -- current outer shape of the polygon
             
  deriving (Eq,Ord,Show)


data Segment a = Segment {  _v0 :: a
                         , _v1 :: a }
  deriving (Eq,Show,Ord)

makeLenses ''Segment

makeLenses ''Paper
-- Exterior vertex is one that is not inside any facet





-- Needs check
-- Create connected line segments
cycleNeighbors ::  Seq Vertex -> Seq SegmentVertex
cycleNeighbors vs = cycleIfLong  
          where
             cycleIfLong
               | Seq.length vs >= 2 = Seq.fromList . reverse . foldl' buildSegments buildOneSegment $ (Seq.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (Seq.index vs 0) (Seq.index vs 1)]
             
             
buildSegments :: [Segment a] -> a -> [Segment a]
buildSegments (seg:segs ) v = (Segment (_v1 seg) v ): seg : segs  
buildSegments [] _ = []



cycleNeighborsIdx ::  Seq Int -> Seq SegmentIdx
cycleNeighborsIdx vs = cycleIfLong  
          where
             cycleIfLong
               | Seq.length vs >= 2 = Seq.fromList . reverse . foldl' buildSegments buildOneSegment $ (Seq.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (Seq.index vs 0) (Seq.index vs 1)]




unsegmentNeighbors :: Seq (Segment a) -> Seq a
unsegmentNeighbors segments = Seq.fromList $ foldl unbuildSegments [(_v1 $ Seq.index segments 0),(_v0 $ Seq.index segments 0)] (Seq.drop 1 segments)
 where
   unbuildSegments vs (Segment _ vnew)  =  vnew : vs



-- Make sure the point is in the polygon
testPointInside :: Bool
testPointInside = (flip pointInside (V2 1.5 0.5) (Seq.fromList [(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]) == False) &&
                 (flip pointInside (V2 0.5 0.5) (Seq.fromList [(V2 0 0), (V2 1 0), (V2 1 1), (V2 0 1)]) == True) 


-- | Note a point that is onto the boundary is not inside it   
pointInside ::  Seq Vertex -> V2 Fraction -> Bool
pointInside  vs (V2 x y) = (Seq.length intersectSegments) `mod` 2 == 1
  where
     intersectSegments = Seq.filter (\p -> positiveXAxis p && aboveBelow p) allSegments
     allSegments = cycleNeighbors vs
     positiveXAxis (Segment (V2 x0 _ ) (V2 x1 _))  =  (x0 > x) || (x1 > x )

     aboveBelow (Segment (V2 _ y0) (V2 _ y1))   = ((y0 > y) && (y1 < y)) ||
                                                  ((y0 < y) && (y1 > y))

fromSeq ::Ord a =>  Seq a -> Set a
fromSeq = Set.fromList . toList

-- | Find all the vertices at the edge of our folds
exteriorVertices :: Paper -> Set Vertex
exteriorVertices paper = allExteriorVertices
  where
    vertexList                                           =  paper ^. vertices
    facetSeq                                             = _facets paper
    allExteriorVertices                                  =  foldr checkVertexAgainstAllFacets Set.empty vertexList            
    convertIndexToVertex vs                              = Seq.index vertexList  <$> vs
    checkVertexAgainstAllFacets vertex exteriorVertexSet = if not (pointInside (convertIndexToVertex  facetSeq) vertex  )
                                                           then Set.insert vertex exteriorVertexSet
                                                           else exteriorVertexSet

   

    
-- | Find all segments whose vertices lay exclusively on the boundaries
-- exteriorSegments :: Paper -> Set Segments
-- exteriorSegments paper = 
--   where    
--     exteriorVertices'              = exteriorVertices paper
--     facetSet                       = facets paper
    



data ValidFold = ValidFold {  vertexIndex :: Int
                           ,targetIndex :: Vertex
                          , segment     :: Segment Int
                          } deriving (Eq, Show,Ord)







-- Fold something


testPaper :: Paper
testPaper = Paper [(V2 0 0), (V2 0 1), (V2 1 1), (V2 1 0)] ([0, 1 ,2 ,3])

-- outputPaperAfterTriFold = (foldPaper testPaper 3 2 ) == (Paper [(V2 0 0), (V2 0 1), (V2 1 1)] ([[0, 1 ,2 ]]))



-- Everything for the fold

-- foldPaper
--  :: Paper
--     -> Int -> Int -> Seq (Seq (Maybe ((Int, Int), V2 Fraction)))


foldPaper :: Paper -> Int -> Int -> Seq (V2 Fraction)
foldPaper paper initialIndex finalIndex = dropInteriorVertices facetVertexed $
                                             reflectOverSegment facetVertexed findCreaseLine
  where
    initialVertex           = Seq.index vertices' initialIndex

    vertices'               = (_vertices paper)
    facetVertexed           = (Seq.index vertices') <$> facet
    facet                   = _facets  paper
    exteriorVertices'       = exteriorVertices paper   -- initial index must be exterior
    creaseLine              = crease paper initialIndex finalIndex

    dropInteriorVertices facet' vs = Seq.filter (not . pointInside facet' ) vs

    findCreaseLine
       | Set.member initialVertex exteriorVertices' =  findExteriorIntersection creaseLine
       | otherwise = error "non exterior vertex"
       
    intersectExteriorSegment cl (Segment i1 i2)
      |(Set.member (Seq.index vertices' i1) exteriorVertices') &&
       (Set.member (Seq.index vertices' i2) exteriorVertices' )   =  (intersectionBetween (Seq.index vertices' i1 ) (Seq.index vertices' i2 ) cl )
      |otherwise = Nothing

    findExteriorIntersection :: LineC -> Segment (V2 Fraction)
    findExteriorIntersection cl = case catMaybes $ toList $ (intersectExteriorSegment cl <$> cycleNeighborsIdx facet) of
        [x0,x1] -> Segment x0 x1
        _       -> error "wrong number of vertices in exterior intersection"

    reflectOverSegment :: (Functor f) =>  f (V2 Fraction) -> Segment (V2 Fraction) -> f (V2 Fraction)
    reflectOverSegment vs (Segment x0 x1)  = shouldBeReflected x0 creaseVector <$> vs
      where
        creaseVector   = x1 - x0
        shouldBeReflected origin' reflectionVector vertex = if (cross2 (vertex - origin') reflectionVector) < 0
                                                            then reflectVertex creaseLine vertex
                                                            else vertex




reflectVertex :: LineC -> V2 Fraction -> V2 Fraction
reflectVertex (Vertical (VLine x1)) (V2 x2 y2)          = V2 (-2 * (x2 - x1) + x2) y2 
reflectVertex (LineC (LineF m b)) (V2 x y) = (scale x colOne) + (scale (y - b) colTwo) + (V2 0 b)
  where
    scalar = (1 / (1 + m*m) )
    colOne = scale scalar (V2 (1 - m * m) (2*m))
    colTwo = scale scalar (V2 (2*m) (m*m - 1) )   



testReflectVertex :: Bool
testReflectVertex = (reflectVertex (LineC (LineF 1 0)) (V2 0 1) ) == (V2 1 0)

proptest :: IO ()
proptest = quickCheck (\pt1 pt2  -> (reflectVertex (LineC (LineF (m + 1) b)) . reflectVertex (LineC (LineF (m + 1) b)) $ (V2 pt1 (pt2+1 + b)) ) == (V2 pt1 (pt2+1 + b)))              where
       m = 3
       b = 8


-- Return the new vertices created by the crease.
-- no check on exterior!
crease :: Paper -> Int -> Int -> LineC
crease paper initialIndex finalIndex  = creaseLine
  where
    initialVertex           = Seq.index (_vertices paper) initialIndex
    finalVertex             = Seq.index (_vertices paper) finalIndex


    directionVertex         = finalVertex - initialVertex
    creasePoint             = -1 * directionVertex /0.5 + finalVertex
    creaseDirection         = perp directionVertex
    creaseLine              = lineEquation (creaseDirection + creasePoint) creasePoint
    
    



segmentIntersection  ::  LineC -> Segment (V2 Fraction) -> Maybe (V2 Fraction)
segmentIntersection  line (Segment vrt1 vrt2) = intersectionBetween vrt1 vrt2 line


-- | Simple Line Equation holder...

data LineF =  LineF {  lineM :: {-# UNPACK  #-} !Fraction,
                      lineB :: {-# UNPACK  #-}!Fraction}


newtype VLine = VLine {xCoord :: Fraction}

data LineC = LineC     LineF
           | Vertical  VLine



-- | Generate a line equation from two Vertices
-- Equal vertices generate a vertical line at the x 
lineEquation :: V2 Fraction -> V2 Fraction -> LineC
lineEquation (V2 x1 y1) (V2 x2 y2) = guardedLineEquation
  where
    m
      | (x1  /= x2) = (y1 - y2) / (x1 - x2)
      | otherwise   = error "x1 x2 coordinates equal"
    b
      | (x1 /= x2)  = (y2*x1 - y1*x2) / (x1 - x2)
      | otherwise   = error "x2 x1 coordinates equal"

    guardedLineEquation
      |x1 /= x2  = LineC (LineF m b)
      |otherwise = Vertical (VLine x1)


-- | Find the intersection point of two lines
intersection :: LineC -> LineC -> V2 Fraction
intersection (Vertical (VLine _)) (Vertical (VLine _)) = error "Two vertical line segments do not intersec" 
intersection (Vertical (VLine x1)) (LineC (LineF m b))   = (V2 x1 (m*x1 + b))
intersection (LineC (LineF m b))   (Vertical (VLine x1)) = (V2 x1 (m*x1 + b))
intersection (LineC (LineF m1 b1)) (LineC (LineF m2 b2)) = (V2 x y)
  where
    x = (b1 - b2) / (m1 - m2)
    y = (b2*m1 - b1*m2) / (m1 -m2 )





-- | check the line formed by two verticies against a line equation
-- to see if the intersection of the two lines falls between said vertices

intersectionBetween  :: V2 Fraction -> V2 Fraction -> LineC -> Maybe (V2 Fraction)
intersectionBetween vrt1 vrt2 linetest = between
    where
      vi = intersection (lineEquation vrt1 vrt2) linetest
      parallelDirections = dot (vi - vrt1) (vi - vrt2) > 0 -- parallel
      between =  if parallelDirections
                    then Nothing    -- parallel direction vectors mean a point not inbetween
                    else (Just vi) 


