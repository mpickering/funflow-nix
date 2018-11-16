{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.Funflow.External.Nix
  ( NixConfig(..)
  , NixpkgsSource(..)
  , Environment(..)
  , nix )where

import           Control.Funflow
import           Path
import           Control.Funflow.ContentHashable
import           Control.Funflow.ContentStore as CS
import           Control.Funflow.External     as E
import           Data.Semigroup                   ((<>))
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Data.List
import           Text.URI (URI)
import qualified Text.URI as URI

data NixpkgsSource = NIX_PATH -- ^ Inherit the @NIX_PATH@ from the environment
                   | NixpkgsTarball URI  -- ^ The 'URI' pointing to a nixpkgs tarball
                   deriving Generic


data NixConfig =
  NixShellConfig {
    environment :: Environment, -- ^ Specification of the nix environment
    nixpkgsSource :: NixpkgsSource, -- ^ Which version of nixpkgs to use
    command :: T.Text, -- ^ The command to run in the environment
    args :: [ParamField], -- ^ Arguments to pass to the command
    env :: [(T.Text, Param)], -- ^ Environmental variables which are set in the environment
    stdout :: OutputCapture
    }

data Environment = ShellFile (Content File) -- ^ Path to a shell.nix file
                 | PackageList [T.Text] -- ^ A list of packages that
                                        -- will be passed by @-p@.
                 deriving Generic

instance ContentHashable IO Environment where

instance ContentHashable IO NixpkgsSource where
  contentHashUpdate ctx NIX_PATH           = contentHashUpdate_fingerprint ctx NIX_PATH
  contentHashUpdate ctx (NixpkgsTarball s) = contentHashUpdate ctx (URI.render s)

nixpkgsSourceToParam :: NixpkgsSource -> Param
nixpkgsSourceToParam NIX_PATH = envParam "NIX_PATH"
nixpkgsSourceToParam (NixpkgsTarball uri) = textParam ("nixpkgs=" <> URI.render uri)

toExternal :: NixConfig -> ExternalTask
toExternal NixShellConfig{..} = ExternalTask
  { _etCommand = "nix-shell"
  , _etParams =
      [ "--run"
      , Param [ParamCmd (Param (intersperse (ParamText " ") (ParamText command : args)))]
      ] ++ packageSpec environment
  , _etEnv = E.EnvExplicit $ ("NIX_PATH", nixpkgsSourceToParam nixpkgsSource ) : env
  , _etWriteToStdOut = stdout
  }
  where
    packageSpec :: Environment -> [Param]
    packageSpec (ShellFile ip) = [contentParam ip]
    packageSpec (PackageList ps) = [textParam ("-p " <> p) | p <- ps]


-- | Constructor a flow from a @NixConfig@
nix :: (ContentHashable IO a, ArrowFlow eff ex arr) => (a -> NixConfig)
                                                    -> arr a CS.Item
nix f = external $ toExternal . f
