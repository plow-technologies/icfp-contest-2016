module Origami.Folding where




import Diagrams.Prelude
import Diagrams.TwoD.Vector
import Diagrams.Segment
import Origami.Silhouette
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Set (Set)
import qualified Data.Set as Set

data Paper = Paper { vertices     :: Vector Vertex
                  , facets       :: Set (Vector VertexIndex)
                  , derivedEdges :: Set NormVertex }

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

--}





foldPaper :: SourcePaper -> VertexToMove -> VertexToTarget -> SinkPaper
foldPaper source vM vT = _
     where
       foldDirection    = getVector vM vT 
       foldCrease       = orthVector foldDirection
       validNewPaper    = findCrease source foldCrease



getVector vM vT =  vT - vM



orthVector   = perp

findCrease  orthoNorm  = _

sourceToSink = _

