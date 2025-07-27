{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module WebApp (runWebApp) where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Control.Monad.IO.Class (liftIO)

import Jinja (renderJinjaSingle, renderJinjaBatch)

-- * api

type JinjaApi =
  "single" :> ReqBody '[JSON] JinjaApiInputSingle :> Post '[JSON] JinjaApiOutput :<|>
  "batch" :> ReqBody '[JSON] JinjaApiInputBatch :> Post '[JSON] [JinjaApiOutput]

jinjaApi :: Proxy JinjaApi
jinjaApi = Proxy

-- * app

runWebApp :: IO ()
runWebApp = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve jinjaApi server

server :: Server JinjaApi
server =
  postRenderSingle :<|>
  postRenderBatch

postRenderSingle :: JinjaApiInputSingle -> Handler JinjaApiOutput
postRenderSingle input = do
  renderResult <- liftIO $ renderJinjaSingle input.template input.context
  case renderResult of
    Left err -> return $ JinjaApiOutput Nothing (Just err)
    Right res -> return $ JinjaApiOutput (Just res) Nothing

postRenderBatch :: JinjaApiInputBatch -> Handler [JinjaApiOutput]
postRenderBatch input = do
  renderResults <- liftIO $ renderJinjaBatch input.template input.contexts
  return $ map (\res -> case res of
    Left err -> JinjaApiOutput Nothing (Just err)
    Right r -> JinjaApiOutput (Just r) Nothing) renderResults

-- * data types

data JinjaApiInputSingle = JinjaApiInputSingle
  { template :: String
  , context :: Value
  } deriving (Generic, Show)

instance ToJSON JinjaApiInputSingle
instance FromJSON JinjaApiInputSingle

data JinjaApiInputBatch = JinjaApiInputBatch
  { template :: String
  , contexts :: [Value]
  } deriving (Generic, Show)

instance ToJSON JinjaApiInputBatch
instance FromJSON JinjaApiInputBatch

data JinjaApiOutput = JinjaApiOutput
  { result :: Maybe String
  , message :: Maybe String
  } deriving (Generic, Show)

instance ToJSON JinjaApiOutput
instance FromJSON JinjaApiOutput
