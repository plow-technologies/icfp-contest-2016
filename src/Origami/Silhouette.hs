{-# LANGUAGE OverloadedStrings #-}
module Origami.Silhouette where

import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text as Text
import           Data.Monoid

import Origami.Numbers

type Polygon = [Coordinate]

type Silhouette = [Polygon]

-- | The edges of a polygon as coordinate pairs
polygonEdges :: Polygon -> [(Coordinate, Coordinate)]
polygonEdges polygon@(start : rest) = zip polygon (rest ++ [start])

-- | Check if a polygon is clockwise (i.e negative area)
polygonClockwise :: Polygon -> Bool
polygonClockwise polygon = (sum $ map edgeSum $ polygonEdges polygon) > 0
  where
    edgeSum (Coordinate x1 y1, Coordinate x2 y2) = (x2 - x1) * (y2 + y1)
  
-- | Parse the size of a polygon
polygonSizeParser :: Int -> Parser [Int]
polygonSizeParser polygonCount = (count polygonCount $ decimal <* skipSpace) <?> "Polygon size"

-- | Parse how many polygons
polygonCountParser :: Parser Int
polygonCountParser = (decimal <* skipSpace) <?> "Polygon count"

-- | Parse a polygon, given the number of coordinates
polygonParser :: Int -> Parser Polygon
polygonParser vertexCount = (count vertexCount $ coordinateParser) <?> "Polygon"

-- | Parse the polygons from a silhouette specification
silhouetteParser :: Parser Silhouette
silhouetteParser = (polygonCountParser >>= polygonSizeParser >>= mapM polygonParser) <?> "Silhouette"

-- | Pretty print a list of polygons
prettySilhouette :: Silhouette -> Text
prettySilhouette polygons =
     Text.unlines
  $  (Text.pack $ show $ length polygons)
  :  (map (Text.pack . show . length) polygons)
  ++ (map (Text.unlines . map prettyCoordinate) polygons)
