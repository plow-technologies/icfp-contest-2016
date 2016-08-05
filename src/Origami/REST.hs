{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Origami.REST where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.Proxy
import Data.IORef
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Network.HTTP.Client hiding (Proxy(..))
import Servant.API
import Servant.Client

type OrigamiAPI = "api"
                  :> Header "Expect" Text
                  :> Header "X-API-Key" Text
                  :> (     "hello" :> Get '[JSON] Value
                      :<|> "blob" :> Capture "hash" Text :> Get '[JSON] Value
                      :<|> "blob" :> Capture "hash" Text :> Get '[PlainText] Text 
                      :<|> "snapshot" :> "list" :> Get '[JSON] Value
                      :<|> "problem" :> "submit" :> ReqBody '[JSON] Value :> Post '[JSON] Value
                      :<|> "solution" :> "submit" :> ReqBody '[JSON] Value :> Post '[JSON] Value
                     )

origamiAPI :: Proxy OrigamiAPI
origamiAPI = Proxy

baseUrl = BaseUrl Http "2016sv.icfpcontest.org" 80 "/"

hello :<|> blobValue :<|> blobText :<|> snapshot :<|> postProblem :<|> postSolution = client origamiAPI (Just "") (Just apiKey)

apiKey :: Text
apiKey = "75-93dbd42d0a532816aab346f3d8c3f9d7"

test :: IO ()
test = do
  manager <- newManager defaultManagerSettings
  result <- runExceptT $ snapshot manager baseUrl
  print result

data RESTConfiguration
  = RESTConfiguration
  { restManager :: Manager
  , restSnapshotRef :: IORef (Maybe Value)
  }

type RESTM a = ReaderT RESTConfiguration ClientM a

runRESTM :: RESTM a -> IO (Either ServantError a)
runRESTM action = do
  manager <- newManager defaultManagerSettings
  snapshotRef <- newIORef Nothing
  runExceptT $ runReaderT action (RESTConfiguration manager snapshotRef)

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
      manager <- asks restManager
      snapshots <- lift $ snapshot manager baseUrl
      case reverse $ snapshots ^.. values of
        [] -> throwError (error "oops")
        (lastShot : _) -> case lastShot ^? key "snapshot_hash" . _String of
                            Nothing -> throwError (error "oops")
                            Just snapshotHash -> do
                              value <- lift $ blobValue snapshotHash manager baseUrl 
                              ref <- asks restSnapshotRef
                              liftIO $ atomicModifyIORef ref (const (Just value, ()))
                              return value
