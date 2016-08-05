{-# LANGUAGE OverloadedStrings #-}
module Origami.Skeleton where

import           Origami.Silhouette

import           Data.Attoparsec.Text         
import           Data.Text            (Text)
import qualified Data.Text         as Text
import           Data.Monoid

type Line = (Coordinate, Coordinate)

type Skeleton = [Line]

lineParser :: Parser Line
lineParser = curry id <$> coordinateParser <* skipSpace <*> coordinateParser

skeletonParser :: Parser Skeleton
skeletonParser = (decimal <* skipSpace) >>= flip count (lineParser <* skipSpace)

prettySkeleton :: Skeleton -> Text
prettySkeleton skeleton = 
    Text.intercalate "\n"
  $ (Text.pack $ show $ length skeleton)
  : map (\(start, end) -> prettyCoordinate start <> " " <> prettyCoordinate end) skeleton
