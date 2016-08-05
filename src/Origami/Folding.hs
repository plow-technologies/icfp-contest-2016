module Origami.Folding where




import Diagrams.Prelude
import Diagrams.TwoD.Vector



data Paper = Paper

type SourcePaper = Paper
type SinkPaper   = Paper

type VertexToTarget = V2 Double
type VertexToMove   = V2 Double


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



orthVector = norm . perp

findCrease = _

sourceToSink = _

