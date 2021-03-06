{-# LANGUAGE OverloadedStrings #-}
module Origami.Solution where

import           Origami.Numbers
import           Data.Text (Text)
import qualified Data.Text as Text

data Solution
  = Solution
  { _solutionPositions :: [Position]
  , _solutionPolygons :: [[Int]]
  }

data Position
  = Position
  { _positionSource :: !Coordinate
  , _positionDestination :: !Coordinate
  }

prettySources :: [Position] -> Text
prettySources positions = 
    Text.unlines
  $ (Text.pack $ show $ length positions)
  : map (prettyCoordinate . _positionSource) positions

prettyPolygons :: [[Int]] -> Text
prettyPolygons polygons =
    Text.unlines
  $ (Text.pack $ show $ length polygons)
  : map (Text.intercalate " " . (\polygon -> (Text.pack $ show $ length polygon) : map (Text.pack . show) polygon)) polygons

prettyDestinations :: [Position] -> Text
prettyDestinations positions =
    Text.unlines
  $ map (prettyCoordinate . _positionDestination) positions

prettySolution :: Solution -> Text
prettySolution (Solution positions polygons) =
  Text.unlines
  [ prettySources positions
  , prettyPolygons polygons
  , prettyDestinations positions
  ]
