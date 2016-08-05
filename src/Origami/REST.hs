{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Origami.REST where

import Servant.API
import Servant.Client
import Data.Text (Text)
import Data.Aeson
import Data.Proxy
import Data.IORef
import Control.Monad.Reader

import Control.Monad.Except
import Network.HTTP.Client hiding (Proxy(..))

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

