{-# LANGUAGE OverloadedStrings #-}
module Origami.Numbers where

import           Control.Applicative ((<|>))
import           Data.Ratio
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text as Text
import           Data.Monoid

-- | Fractional type over 'Int'
type Fraction = Ratio Int

-- | Parse a fraction from numerator / denominator notation
fractionParser :: Parser Fraction
fractionParser =
  (    (%)
   <$> decimal
   <*> (("/" *> decimal) <|> pure 1))
  <?> "Fraction"

-- | Pretty-print a fraction
prettyFraction :: Fraction -> Text
prettyFraction fraction = (Text.pack $ show $ numerator fraction) <> "/" <> (Text.pack $ show $ denominator fraction)

-- | Strict pair of 'Fraction' for coordinates
data Coordinate
  = Coordinate
  { _x :: !Fraction
  , _y :: !Fraction
  }

-- | Parse a coordinate from x,y notation
coordinateParser :: Parser Coordinate
coordinateParser =
  (    Coordinate
   <$> fractionParser
   <*  ","
   <*> fractionParser
   <*  skipSpace)
  <?> "Coordinate"

-- | Pretty-print a coordinate
prettyCoordinate :: Coordinate -> Text
prettyCoordinate (Coordinate x y) = prettyFraction x <> "," <> prettyFraction y
