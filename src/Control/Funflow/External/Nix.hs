{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
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
                 | PackageList [T.Text] -- ^ Path to a list of packages that
                                        -- will be passed by `-p`.
                 deriving Generic

instance ContentHashable IO Environment where

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
    packageSpec (PackageList ps) = [textParam ("-p " <> p) | p <- ps]


nix :: (ContentHashable IO a, ArrowFlow eff ex arr) => (a -> NixConfig)
                                                    -> arr a CS.Item
nix f = external $ toExternal . f
