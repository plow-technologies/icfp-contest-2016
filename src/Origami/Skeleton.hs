{-# LANGUAGE OverloadedStrings #-}
module Origami.Skeleton where

import           Origami.Numbers

import           Data.Attoparsec.Text         
import           Data.Text            (Text)
import qualified Data.Text         as Text
import           Data.Monoid

type Line = (Coordinate, Coordinate)

type Skeleton = [Line]

lineParser :: Parser Line
lineParser = (curry id <$> coordinateParser <*> coordinateParser) <?> "Line"

skeletonParser :: Parser Skeleton
skeletonParser = ((decimal <* skipSpace) >>= flip count lineParser) <?> "Skeleton"

prettySkeleton :: Skeleton -> Text
prettySkeleton skeleton = 
    Text.intercalate "\n"
  $ (Text.pack $ show $ length skeleton)
  : map (\(start, end) -> prettyCoordinate start <> " " <> prettyCoordinate end) skeleton
