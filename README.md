`funflow-nix` provides functions for creating flows which run in a nix
environment.

The library exposes the `NixConfig` data type which allows you to specify the
environment and command to run. This is then turned into a flow using `nix`.

A complete example can be seen in `examples/Simple.hs`.

We can pin the version of nixpkgs we want to use by specifying a tarball to
use as the source.

```
tarballSource :: NixpkgsSource
tarballSource = NixpkgsTarball [uri|https://github.com/NixOS/nixpkgs/archive/a19357241973538212b5cb435dde84ad25cbe337.tar.gz|]

nixConfig :: Environment -> NixConfig
nixConfig senv =
  NixShellConfig {
    environment = senv
    , command = "jq"
    , args = [ParamText "--version"]
    , env = []
    , stdout = StdOutCapture
    , nixpkgsSource = tarballSource
  }
```

Once the config has been specified. It can be turned into a flow by using the
`nix` function.


```
jqVersionPkg :: SimpleFlow () String
jqVersionPkg = readString_ <<< nix (\() -> nixConfig (PackageList ["jq"]))
```

