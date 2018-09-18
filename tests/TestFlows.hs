{-# LANGUAGE Arrows            #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module TestFlows where

import           Control.Arrow
import           Control.Concurrent.Async                    (withAsync)
import           Control.Exception.Safe                      hiding (catch)
import           Control.Funflow
import           Control.Funflow.ContentHashable
import qualified Control.Funflow.ContentStore                as CS
import           Control.Funflow.External.Coordinator.Memory
import           Control.Funflow.External.Executor           (executeLoop)
import           Path
import           Path.IO
import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Funflow.External.Nix

data FlowAssertion where
  FlowAssertion :: (Eq b, Show b)
                => String -- test name
                -> a  -- input
                -> SimpleFlow a b -- the flow to test
                -> Maybe b --expected output - Nothing for expected failure
                -> IO () -- test setup action
                -> FlowAssertion

mkError :: String -> SomeException
mkError = toException . userError

nixConfig :: Environment -> NixConfig
nixConfig senv =
  NixConfig {
    environment = senv
    , command = "jq"
    , args = [ParamText "--version"]
    , env = []
    , stdout = StdOutCapture
  }

-- | Test that we can merge directories within the content store.
jqVersion :: SimpleFlow () String
jqVersion = proc () -> do
  cwd <- stepIO (const getCurrentDir) -< ()
  shellScript <- copyFileToStore
                    -< (FileContent (cwd </> [relfile|data/shell.nix|]),[relfile|data/shell.nix|])
  readString_ <<< nix nixConfig -< ShellFile shellScript

jqVersionPkg :: SimpleFlow () String
jqVersionPkg = readString_ <<< nix (\() -> nixConfig (PackageList ["jq"]))


flowAssertions :: [FlowAssertion]
flowAssertions =
  [ FlowAssertion "shell 1" () jqVersion (Just "jq-1.5\n") (return ())
  , FlowAssertion "shell 2" () jqVersionPkg (Just "jq-1.5\n") (return ())
  ]

testFlowAssertion :: FlowAssertion -> TestTree
testFlowAssertion (FlowAssertion nm x flw expect before) =
  testCase nm $
    withSystemTempDir "test_output_" $ \storeDir ->
    CS.withStore storeDir $ \store -> do
      hook <- createMemoryCoordinator
      before
      res <- withAsync (executeLoop MemoryCoordinator hook store) $ \_ ->
        runSimpleFlow MemoryCoordinator hook store flw x
      assertFlowResult expect res

assertFlowResult :: (Eq a, Show ex, Show a) => Maybe a -> Either ex a -> Assertion
assertFlowResult expect res =
    case (expect, res) of
      (Nothing, Left _) -> return ()
      (Just xr, Right y) -> assertEqual "flow results" xr y
      (Nothing, Right y) -> assertFailure $ "expected flow failure, got success" ++ show y
      (Just xr, Left err) -> assertFailure $ "expected success "++ show xr++", got error" ++ show err

tests :: TestTree
tests = testGroup "Flow Assertions" $ map testFlowAssertion flowAssertions
