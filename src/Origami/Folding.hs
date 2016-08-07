{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
module Origami.Folding where




import Diagrams.Prelude hiding (Segment,Color)
import Diagrams.TwoD.Vector

import Origami.Numbers





import qualified Data.Sequence as Seq
import Data.Sequence (Seq)


import Data.Foldable (toList,foldl')

import Data.Maybe (catMaybes,fromMaybe)
import Data.Monoid ((<>))

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

data Crease        = Crease { _reflectedIndex :: Seq VertexIndex
                            ,_creaseSegment  :: SegmentIdx }
  deriving (Eq,Ord,Show)

data Facet = Facet { _facetOutline :: Seq VertexIndex}

data Paper = Paper { _vertices      :: Vertices
                  , _outline       :: (Seq VertexIndex) }  -- current outer shape of the polygon

  deriving (Eq,Ord,Show)

data Layer = Layer { _paperLayer :: Paper,
                    _paperCrease :: Maybe Crease

                   }
  deriving (Eq,Show,Ord)
-- A very merry unpaper to you!
-- | Unpaper has the paper final and initial
-- It has a list of creases that have been made in the paper
-- Each new crease is added at the head so they can be popped off
data UnPaper = UnPaper { _paperOut      :: Paper,
                       _paperIn        :: Paper,
                       _creaseST         :: Crease
                       } -- use indexes to help ensure tied to paper
  deriving (Eq,Ord,Show)
data Segment a = Segment {  _v0 :: a
                         , _v1 :: a }
  deriving (Eq,Show,Ord)

makeLenses ''Crease

makeLenses '' Layer 
makeLenses ''Segment

makeLenses ''Paper

makeLenses ''UnPaper
-- Exterior vertex is one that is not inside any facet


{-
fromList [V2 (0 % 1) (0 % 1)   , V2 (0 % 1) (1 % 1)    ,V2 (1 % 1) (1 % 1)    ,V2 (1 % 1) (0 % 1)   ]
fromList [V2 (0 % 1) ((-2) % 1), V2 (0 % 1) ((-3) % 1) ,V2 (1 % 1) ((-3) % 1) ,V2 (1 % 1) ((-2) % 1)]
λ> 

-}


-- Needs check
-- Create connected line segments
cycleNeighbors ::  Seq Vertex -> Seq SegmentVertex
cycleNeighbors vs = cycleIfLong  
          where
             cycleIfLong
               | Seq.length vs >= 2 = Seq.fromList . reverse . firstAndLast . foldl' buildSegments buildOneSegment $ (Seq.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (Seq.index vs 0) (Seq.index vs 1)]
             firstAndLast segs = (Segment (_v1 . head $ segs ) (Seq.index vs 0) ) : segs



buildSegments :: [Segment a] -> a -> [Segment a]
buildSegments (seg:segs ) v = (Segment (_v1 seg) v ): seg : segs  
buildSegments [] _ = []



cycleNeighborsIdx ::  Seq Int -> Seq SegmentIdx
cycleNeighborsIdx vs = cycleIfLong  
          where
             cycleIfLong
               | Seq.length vs >= 2 = Seq.fromList . reverse . firstAndLast . foldl' buildSegments buildOneSegment $ (Seq.drop 2 vs)
               | otherwise = error "length should be at least 2 for a segment"
             buildOneSegment = [Segment (Seq.index vs 0) (Seq.index vs 1)]
             firstAndLast segs = (Segment (_v1 . head $ segs ) (Seq.index vs 0) ) : segs



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
    facetSeq                                             = _outline paper
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


foldPaper paper creaseLine = newUnPaper & paperOut . vertices %~ dropInteriorVertices facetVertexed
  where
    idx                            = Seq.index vertices'
    vertices'                      = _vertices paper
    facetVertexed                  = Seq.index vertices' <$> facets'
    facets'                        = _outline  paper
    exteriorVertices'              = exteriorVertices paper   -- initial index must be exterior
    creaseSegment'                 = findCreaseSegment
    newPaper                       = paper & vertices %~ (\s -> s |> _v0 creaseSegment' |> _v1 creaseSegment')
    (Just indexCreaseSegment)             = Segment <$> (Seq.elemIndexR (_v0 creaseSegment') (newPaper ^. vertices)) <*> (Seq.elemIndexR (_v1 creaseSegment') (newPaper ^. vertices)) 
    dropInteriorVertices facet' vs = Seq.filter (not . pointInside facet' ) vs

    
    newUnPaper = foldr (\(rIdx, newVert) unPaper ->
                           maybe unPaper
                                 (\i ->
                                  unPaper & paperOut.vertices . ix i .~ newVert & creaseST . reflectedIndex %~ \s -> i <| s)
                                 rIdx )                     
                       (UnPaper newPaper paper (Crease Seq.empty indexCreaseSegment) )
                       (reflectOverSegment facets' creaseSegment')
    findCreaseSegment =  findExteriorIntersection creaseLine

    intersectExteriorSegment cl (Segment i1 i2)
      |(Set.member (Seq.index vertices' i1) exteriorVertices' )  &&
       (Set.member (Seq.index vertices' i2) exteriorVertices' )   =  (intersectionBetween (Seq.index vertices' i1 )
                                                                                          (Seq.index vertices' i2 )
                                                                                          cl )
      |otherwise = Nothing

    findExteriorIntersection :: LineC -> Segment (V2 Fraction)
    findExteriorIntersection cl = case catMaybes $ toList $ fromSeq $ (intersectExteriorSegment cl <$> cycleNeighborsIdx facets') of
        [x0,x1] -> Segment x0 x1
        intersectionPoints       -> error $ "wrong number of vertices in exterior intersection " ++
                                            (show intersectionPoints) ++ (show cl)                           
                                            

    reflectOverSegment :: (Functor f) =>  f Int -> Segment (V2 Fraction) -> f ((Maybe Int), V2 Fraction)
    reflectOverSegment vs (Segment x0 x1)  = shouldBeReflected x0 creaseVector <$> vs
      where
        creaseVector   = x1 - x0
        shouldBeReflected origin'
                          reflectionVector
                          vertexIdx = if (cross2 ((idx vertexIdx) - origin') reflectionVector) < 0
                                      then (Just vertexIdx, reflectVertex creaseLine (idx vertexIdx))
                                      else (Nothing , (idx vertexIdx))

-- | given an unpaper.  Decompose into layers.
-- after a crease, the new crease vertex takes the place of the folded vertex
-- in the outline for the bottom  layer
--
-- 
toLayers unpaper = ()
  where
    vertices1 = unpaper ^. paperOut . vertices
    vertices0 = unpaper ^. paperIn . vertices
    outline1  = unpaper ^. paperOut . outline
    outline0  = unpaper ^. paperIn . outline

    crease'    = unpaper ^. creaseST
    reflected  = crease' ^. reflectedIndex




reflectVertex :: LineC -> V2 Fraction -> V2 Fraction
reflectVertex (Vertical (VLine x1)) (V2 x2 y2)          = V2 (-2 * (x2 - x1) + x2) y2 
reflectVertex (LineC (LineF m b))   (V2 x y)            = (scale' x colOne) + (scale' (y - b) colTwo) + (V2 0 b)
  where
    scale' s (V2 x' y') = (V2 (x'*s) (y'*s))
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
    creasePoint             = -1 * directionVertex * 0.5 + finalVertex
    creaseDirection         = perp directionVertex
    creaseLine              = lineEquation (creaseDirection + creasePoint) creasePoint





segmentIntersection  ::  LineC -> Segment (V2 Fraction) -> Maybe (V2 Fraction)
segmentIntersection  line (Segment vrt1 vrt2) = intersectionBetween vrt1 vrt2 line


-- | Simple Line Equation holder...

data LineF =  LineF {  lineM :: {-# UNPACK  #-} !Fraction,
                      lineB :: {-# UNPACK  #-}!Fraction}
 deriving (Show,Ord,Eq)

newtype VLine = VLine {xCoord :: Fraction}
  deriving (Show,Ord,Eq)
data LineC = LineC     LineF
           | Vertical  VLine
  deriving (Show,Ord,Eq)


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
intersection :: LineC -> LineC -> Maybe (V2 Fraction)
intersection (Vertical (VLine _)) (Vertical (VLine _)) = Nothing
intersection (Vertical (VLine x1)) (LineC (LineF m b))   = Just (V2 x1 (m*x1 + b))
intersection (LineC (LineF m b))   (Vertical (VLine x1)) = Just (V2 x1 (m*x1 + b))
intersection (LineC (LineF m1 b1)) (LineC (LineF m2 b2)) = guardedIntersection
  where
    x = (b1 - b2) / (m1 - m2)
    y = (b2*m1 - b1*m2) / (m1 -m2 )
    guardedIntersection
      | m1 /= m2  = Just (V2 x y)
      | otherwise = Nothing




-- | check the line formed by two verticies against a line equation
-- to see if the intersection of the two lines falls between said vertices
-- the dot product of two vectors which start at different points on a line
-- and end at the point under test, will be negative if the vertex lies between them.
-- 
intersectionBetween  :: V2 Fraction -> V2 Fraction -> LineC -> Maybe (V2 Fraction)
intersectionBetween vrt1 vrt2 linetest = between
    where
      maybeVi               = intersection (lineEquation vrt1 vrt2) linetest
      parallelDirections vi = dot (vi - vrt1) (vi - vrt2) < 0 -- antiparallel
      between               =  case (fromMaybe False $ parallelDirections <$> maybeVi) ||
                                    (pointOnLine linetest vrt1)                       ||
                                    (pointOnLine linetest vrt2) of       
                                  True    -> maybeVi
                                  False   -> Nothing




pointOnLine :: LineC              -> V2 Fraction  -> Bool
pointOnLine    (LineC (LineF m b))   (V2 x y)   = y  == (m*x + b)
pointOnLine    (Vertical (VLine x')) (V2 x _)   = x' == x
