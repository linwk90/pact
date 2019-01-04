module ClientSpec (spec) where

import Data.Aeson
import Test.Hspec
import Utils.TestRunner
import qualified Network.HTTP.Client as HTTP
import qualified Control.Exception as Exception
-- import Control.Concurrent.Async

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)
import Pact.Server.Client
import Servant.Client
import qualified Data.HashMap.Strict as HM

_testLogDir, _testConfigFilePath, _testPort, _serverPath :: String
_testLogDir = testDir ++ "test-log/"
_testConfigFilePath = testDir ++ "test-config.yaml"

_testPort = "8080"
_serverPath = "http://localhost:" ++ _testPort

bracket :: IO a -> IO a
bracket action = Exception.bracket (startServer _testConfigFilePath) stopServer (const action)

simpleServerCmd :: IO (Command Text)
simpleServerCmd = do
  simpleKeys <- genKeys
  cmd <- mkExec  "(+ 1 2)" Null Nothing
             [simpleKeys] (Just "test1")
  pure cmd

spec :: Spec
spec = around_ bracket $ describe "tests Servant API client" $ do
  mgr <- runIO $ HTTP.newManager HTTP.defaultManagerSettings
  url <- runIO $ parseBaseUrl _serverPath
  let clientEnv = mkClientEnv mgr url
  it "correctly runs a simple command publicly" $ do
    cmd <- simpleServerCmd
    res <- runClientM (send (SubmitBatch [cmd])) clientEnv
    res `shouldBe` (Right (ApiSuccess (RequestKeys [(cmdToRequestKey cmd)])))
  it "incorrectly runs a simple command privately" $ do
    cmd <- simpleServerCmd
    res <- runClientM (private (SubmitBatch [cmd])) clientEnv
    res `shouldBe` (Right (ApiFailure "Send private: payload must have address"))
  it "correctly runs a simple command publicly and polls the result" $ do
    cmd <- simpleServerCmd
    res <- runClientM (send (SubmitBatch [cmd])) clientEnv
    let rk = cmdToRequestKey cmd
    res `shouldBe` (Right (ApiSuccess (RequestKeys [rk])))
    res' <- runClientM (poll (Poll [rk])) clientEnv
    let cmdData = (toJSON . CommandSuccess . Number) 3
    res' `shouldBe` (Right (ApiSuccess (PollResponses (HM.singleton rk (ApiResult cmdData (Just 0) Nothing)))))
