{-# LANGUAGE OverloadedStrings #-}
module Origami.Silhouette where

import           Data.Ratio
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text as Text
import           Data.Monoid

-- | Fractional type over 'Int'
type Fraction = Ratio Word

-- | Strict pair of 'Fraction' for coordinates
data Coordinate
  = Coordinate
  { _x :: !Fraction
  , _y :: !Fraction
  }

-- | Strict monomorphic list of 'Coordinate', representing a polygon
data Polygon
  = EmptyPoly
  | ExtendPoly !Coordinate !Polygon

-- | Convert a list of coordinates to a polygon
coordinateListToPolygon :: [Coordinate] -> Polygon
coordinateListToPolygon = foldr ExtendPoly EmptyPoly

-- | Convert a polygon to a list of coordinates
polygonToCoordinateList :: Polygon -> [Coordinate]
polygonToCoordinateList EmptyPoly = []
polygonToCoordinateList (ExtendPoly point polygon) = point : polygonToCoordinateList polygon

-- | Parse a fraction from numerator / denominator notation
fractionParser :: Parser Fraction
fractionParser = (%) <$> decimal <* "/" <*> decimal

-- | Parse a coordinate from x,y notation
coordinateParser :: Parser Coordinate
coordinateParser =
      Coordinate
  <$> fractionParser
  <*  ","
  <*> fractionParser
  <*  skipSpace
  
-- | Parse the size of a polygon
polygonSizeParser :: Int -> Parser [Int]
polygonSizeParser polygonCount = count polygonCount $ decimal <* skipSpace 

-- | Parse how many polygons
polygonCountParser :: Parser Int
polygonCountParser = decimal <* skipSpace

-- | Parse a polygon, given the number of coordinates
polygonParser :: Int -> Parser Polygon
polygonParser vertexCount = coordinateListToPolygon <$> (count vertexCount $ coordinateParser)

-- | Parse the polygons from a silhouette specification
polygonsParser :: Parser [Polygon]
polygonsParser = polygonCountParser >>= polygonSizeParser >>= mapM polygonParser

-- | The number of vertices in a polygon
polygonLength :: Polygon -> Int
polygonLength EmptyPoly = 0
polygonLength (ExtendPoly _ polygon) = succ $ polygonLength polygon

-- | Pretty-print a fraction
prettyFraction :: Fraction -> Text
prettyFraction fraction = (Text.pack $ show $ numerator fraction) <> "/" <> (Text.pack $ show $ denominator fraction)

-- | Pretty-print a coordinate
prettyCoordinate :: Coordinate -> Text
prettyCoordinate (Coordinate x y) = prettyFraction x <> "," <> prettyFraction y

-- | Pretty print a list of polygons
prettyPolygons :: [Polygon] -> Text
prettyPolygons polygons =
     Text.intercalate "\n"
  $  (Text.pack $ show $ length polygons)
  :  (map (Text.pack . show . polygonLength) polygons)
  ++ (map (Text.intercalate "\n" . map prettyCoordinate . polygonToCoordinateList) polygons)
