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

type PactServerAPI =
       "api" :> "v1" :>
      (    "send" :> Post '[JSON] RequestKeys
      :<|> "private" :> Post '[JSON] RequestKeys
      :<|> "poll" :> Post '[JSON] PollResponses
      :<|> "listen" :> Post '[JSON] ApiResult
      :<|> "local" :> Post '[JSON] Value
      )
  :<|> "verify" :> Post '[JSON] Value

pactServerAPI :: Proxy PactServerAPI
pactServerAPI = Proxy

send :: ClientM RequestKeys
private :: ClientM RequestKeys
poll :: ClientM PollResponses
listen :: ClientM ApiResult
local :: ClientM Value
verify :: ClientM Value

(send :<|> private :<|> poll :<|> listen :<|> local) :<|> verify = client pactServerAPI
