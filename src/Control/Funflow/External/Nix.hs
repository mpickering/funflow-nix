{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Funflow.External.Nix where

import           Control.Arrow                    (Kleisli (..), second)
import           Control.Funflow
import           Path
import           Control.Funflow.ContentHashable
import           Control.Funflow.ContentStore as CS
import           Control.Funflow.External
import           Data.Semigroup                   (Semigroup, (<>))
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Data.List

data NixpkgsSource = NIX_PATH

data NixConfig =
  NixConfig {
    environment :: Environment,
    command :: T.Text,
    args :: [ParamField],
    env :: [(T.Text, Param)],
    stdout :: OutputCapture }

data Environment = ShellFile (Content File) -- ^ Path to a shell.nix file
                 | PackageList [String] -- ^ Path to a list of packages that
                                        -- will be passed by `-p`.

toExternal :: NixConfig -> ExternalTask
toExternal NixConfig{..} = ExternalTask
  { _etCommand = "nix-shell"
  , _etParams =
      [ "--run"
      , Param [ParamCmd (Param (intersperse (ParamText " ") (ParamText command : args)))]
      ] ++ packageSpec environment
  , _etEnv = ("NIX_PATH", envParam "NIX_PATH") : env
  , _etWriteToStdOut = stdout
  }
  where
    packageSpec :: Environment -> [Param]
    packageSpec (ShellFile ip) = [contentParam ip]

nix :: (ContentHashable IO a, ArrowFlow eff ex arr) => (a -> NixConfig)
                                                    -> arr a CS.Item
nix f = external $ toExternal . f
