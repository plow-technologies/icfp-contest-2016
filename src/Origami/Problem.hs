{-# LANGUAGE OverloadedStrings #-}
module Origami.Problem where

import           Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid

import           Origami.Silhouette
import           Origami.Skeleton

data Problem
  = Problem
  { _silhouette :: Silhouette
  , _skeleton :: Skeleton
  }

parseProblem :: Parser Problem
parseProblem = Problem <$> silhouetteParser <*> skeletonParser

prettyProblem :: Problem -> Text
prettyProblem (Problem silhouette skeleton) = prettySilhouette silhouette <> " " <> prettySkeleton skeleton
