{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Pact.Server.Client
  ( send
  , private
  , poll
  , listen
  , local
  , verify
  ) where

import Data.Aeson
import Data.Proxy
import Servant.API
import Servant.Client
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)

type PactServerAPI =
       "api" :> "v1" :>
      (    "send" :> ReqBody '[JSON] SubmitBatch :> Post '[JSON] (ApiResponse RequestKeys)
      :<|> "private" :> ReqBody '[JSON] SubmitBatch :> Post '[JSON] (ApiResponse RequestKeys)
      :<|> "poll" :> ReqBody '[JSON] Poll :> Post '[JSON] (ApiResponse PollResponses)
      :<|> "listen" :> ReqBody '[JSON] ListenerRequest :> Post '[JSON] ApiResult
      :<|> "local" :> ReqBody '[JSON] (Command Text) :> Post '[JSON] Value
      )
  :<|> "verify" :> ReqBody '[JSON] Value :> Post '[JSON] Value

pactServerAPI :: Proxy PactServerAPI
pactServerAPI = Proxy

send :: SubmitBatch -> ClientM (ApiResponse RequestKeys)
private :: SubmitBatch -> ClientM (ApiResponse RequestKeys)
poll :: Poll -> ClientM (ApiResponse PollResponses)
listen :: ListenerRequest -> ClientM ApiResult
local :: Command Text -> ClientM Value
verify :: Value -> ClientM Value

(send :<|> private :<|> poll :<|> listen :<|> local) :<|> verify = client pactServerAPI
