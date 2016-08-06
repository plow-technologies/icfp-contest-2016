{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe

import Origami.Silhouette
import Origami.Skeleton
import Origami.Solution
import Origami.REST

main :: IO ()
main =
  void $ runRESTM $ forever $ do
    status <- getSnapshot
    maybe (return ()) (liftIO . print)
      $ do
           let users = status ^.. key "leaderboard" . values    
           us <- listToMaybe $ filter ((== Just "75") . (^? key "username" . _String)) users
           us ^? key "score" . _Double
    liftIO $ threadDelay 10000000
