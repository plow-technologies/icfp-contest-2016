{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Origami.REST where

import           Control.Arrow (left)
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Attoparsec.Text
import           Data.ByteString.Lazy (toStrict)
import           Data.IORef
import           Data.Maybe (mapMaybe)
import           Data.Proxy
import           Data.Text (Text)
import           Data.Text.IO       as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Network.HTTP.Client hiding (Proxy(..))
import qualified Network.HTTP.Media as Media
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.Client

import Origami.Problem

type OrigamiAPI = "api"
                  :> Header "Expect" Text
                  :> Header "X-API-Key" Text
                  :> (     "hello" :> Get '[JSON] Value
                      :<|> "blob" :> Capture "hash" Text :> Get '[JSON] Value
                      :<|> "blob" :> Capture "hash" Text :> Get '[PlainPlainText] Text 
                      :<|> "snapshot" :> "list" :> Get '[JSON] Value
                      :<|> "problem" :> "submit" :> ReqBody '[JSON] Value :> Post '[JSON] Value
                      :<|> "solution" :> "submit" :> ReqBody '[JSON] Value :> Post '[JSON] Value
                     )

origamiAPI :: Proxy OrigamiAPI
origamiAPI = Proxy

baseUrl = BaseUrl Http "2016sv.icfpcontest.org" 80 "/"

hello :<|> blobValue :<|> blobText :<|> snapshot :<|> postProblem :<|> postSolution
  = client origamiAPI (Just "") (Just apiKey)

apiKey :: Text
apiKey = "75-93dbd42d0a532816aab346f3d8c3f9d7"

test :: IO ()
test = do
  result <- runRESTM getSnapshot
  print result

data RESTConfiguration
  = RESTConfiguration
  { restManager :: Manager
  , restSnapshotRef :: IORef (Maybe Value)
  , restDelayMVar :: MVar ()
  }

type RESTM a = ReaderT RESTConfiguration ClientM a

-- | Internal: use the delay and connection managers from the RESTM monad on a servant client function
asRESTM :: (Manager -> BaseUrl -> ClientM a) -> RESTM a
asRESTM action = do
  manager <- asks restManager
  delayMVar <- asks restDelayMVar
  liftIO $ takeMVar delayMVar
  lift $ action manager baseUrl

-- | Run a RESTM action with a manager and a delay manager. This should be just inside 'main'
runRESTM :: RESTM a -> IO (Either ServantError a)
runRESTM action = do
  manager <- newManager defaultManagerSettings
  snapshotRef <- newIORef Nothing
  delayMVar <- newMVar ()
  forkIO $ forever $ do threadDelay 1000000; putMVar delayMVar () 
  runExceptT $ runReaderT action (RESTConfiguration manager snapshotRef delayMVar)

-- | Get the current contest snapshot, fetching it from the server if its outdated
getSnapshot :: RESTM Value
getSnapshot = do
  ref <- asks restSnapshotRef
  (liftIO $ readIORef ref) 
    >>= maybe
         fetchSnapshot
         (\value -> case value ^? key "snapshot_time" . _Integral of
                      Nothing -> fetchSnapshot
                      Just time -> do
                          currentTime <- liftIO $ getPOSIXTime
                          if fromIntegral time + 3600 < currentTime
                            then fetchSnapshot
                            else return value)
  where
    fetchSnapshot = do
      snapshots <- asRESTM snapshot
      case reverse $ snapshots ^.. key "snapshots" . values of
        [] -> throwError (error "oops1")
        (lastShot : _) -> case lastShot ^? key "snapshot_hash" . _String of
                            Nothing -> throwError (error "oops2")
                            Just snapshotHash -> do
                              value <- asRESTM $ blobValue snapshotHash
                              ref <- asks restSnapshotRef
                              liftIO $ atomicModifyIORef ref (const (Just value, ()))
                              return value

-- | Get the current problem set as ids and hashes. To get the problem itself, pass the has to
-- 'getProblem'
getProblems :: RESTM [(Int, Text)]
getProblems = do
  snapshot <- getSnapshot
  return 
    $ mapMaybe 
      (\value -> do
        problemId <- value ^? key "problem_id" . _Integral
        hash <- value ^? key "problem_spec_hash" . _String
        return (problemId, hash))
    $ snapshot ^.. key "problems" . values        

-- | Get a problem from the hash of its description
getProblem :: Text -> RESTM Problem
getProblem hash = do
  problemText <- asRESTM $ blobText hash
  either 
      (throwError . error)
      return   
    $ eitherResult $ finish $ parse parseProblem problemText
  where
    finish (Partial f) = f ""
    finish result = result

data PlainPlainText deriving Typeable

instance Accept PlainPlainText where
  contentType _ = "text" Media.// "plain"

instance MimeUnrender PlainPlainText Text where
  mimeUnrender _ = left show . Text.decodeUtf8' . toStrict
