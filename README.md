# cabal-audit

cabal-audit is a command line tool to list the external declarations used in a given library.
The goal is to inspect the build dependencies with the
[security-advisories](https://github.com/haskell/security-advisories).


## Overview and scope

This project is composed of a few packages:

- [cabal-audit-plugin](./cabal-audit-plugin): collect dependencies using a plugin.
- [cabal-audit-command](./cabal-audit-command): process the collected dependencies.
- [cabal-audit-test](./cabal-audit-test): a bunch of modules for testing.

- [cabal-audit-hie](./cabal-audit-hie): an investigation to collect dependencies through `.hie` files.
- [cabal-audit-hi](./cabal-audit-hi): an investigation to collect dependencies through `.hi` files built with `-fwrite-if-simplified-core`

The scope of this project is to alert the user when a vulnerable function is being used.
Searching for individual function is particularly important to avoid false alarm when a
given vulnerability only appears in a rarely used declaration of a popular package.


## Usage

### Build the cabal-audit-plugin

Run the following command to get the plugin location (pluginSoPath):

```ShellSession
$ cabal build cabal-audit-plugin
$ pluginSoPath=$(realpath $(find dist-newstyle/ -name "libcabal-audit-plugin.so" | tail -n 1))
```

> Note that this process is heavily inspired by [ghc-wpc](https://github.com/grin-compiler/ghc-whole-program-compiler-project#build)

### Collect the dependencies

The full dependencies call graph needs to be computed before it can be analyzed.
This can be done by enabling the cabal-audit-plugin globally to produce `.hix` files
with the relevant information (written next to the `.hi` files).

#### cabal

Add the following lines to your project's `cabal.project`:

```
package *
  ghc-options:
    -fplugin-trustworthy
    -fplugin-library='${pluginSoPath};cabal-audit-plugin;CabalAudit.Plugin;[]'"
```

#### stack

Add the following lines to your project's `stack.yaml`:

```
apply-ghc-options: everything
ghc-options:
  "$everything":
    -fplugin-trustworthy
    -fplugin-library='${pluginSoPath};cabal-audit-plugin;CabalAudit.Plugin;[]'"
```

#### nix

Use the following nix flake setup:

```nix
{
  nixConfig.bash-prompt = "[nix(cabal-audit)] ";
  inputs = {
    cabal-audit.url = "github:TristanCacqueray/cabal-audit";
  };

  outputs = { self, cabal-audit }:
    let
      # patch all dependencies to be built with the plugin:
      haskellPackages = your-haskell-package-set.extend canal-audit.pluginExtend
    in {
      devShells = your-dev-shell {
        # add the cabal-audit command to the develop shell:
        buildInputs = [haskellPackages.cabal-audit-command]
      }
    }
}
```

### Use the cabal-audit command

Analyze your build with the following command:

```
$ cabal run cabal-audit-command -- --help
cabal-audit - detects uses of known vulnerabilities

Usage: cabal-audit [--extra-lib-dirs DIR] [--write-graph FILENAME]
                   [--target DECLARATION] MODULE...

Available options:
  --extra-lib-dirs DIR     Search module dependencies in DIR (e.g. for ghc
                           librarires)
  --write-graph FILENAME   Dump nodes.tsv and edges.tsv files
  --target DECLARATION     Check if a declaration is reachable
  -h,--help                Show this help text
```

Note that you need to list the exposed modules (your roots). The command generates a edges.tsv file that can be visualized with gephi.
For example, the CabalAudit.Test.External module looks like this:

![cabal-audit](https://github.com/TristanCacqueray/cabal-audit/assets/154392/fc7d42b0-4b32-447b-aa00-6fc2254d5a43)


## Demo

Given this module:

```haskell
module CabalAudit.Test.Simple where

afficheNombre :: Int -> IO ()
afficheNombre 0 = pure ()
afficheNombre n = putStrLn (show n)

maFonction :: String -> IO ()
maFonction = putStr
```

Run the following command:

```ShellSession
$ cabal run cabal-audit-command -- CabalAudit.Test.Simple
cabal-audit-test-0.1-inplace:CabalAudit.Test.Simple.afficheNombre: base:GHC.Base.pure, base:GHC.Base.$fApplicativeIO, base:GHC.Show.show, base:GHC.Show.$fShowInt, base:System.IO.putStrLn, ghc-prim:GHC.Tuple.Prim.()
cabal-audit-test-0.1-inplace:CabalAudit.Test.Simple.maFonction: base:System.IO.putStr
```

## Roadmap

- [x] Collect external declarations.
- [x] Analyze build dependencies.
- [ ] Analyze whole package (e.g. automatically discover the list of exposed modules)
- [ ] Implement the `cabal-audit` command with the advisory-advisories database.

## Note about ghc libraries

To get [ghc libraries](https://downloads.haskell.org/~ghc/9.6.2/docs/libraries/index.html) dependencies:

### Using the plugin

TODO

### Using `.hie` file

Build ghc with this [patch](https://gitlab.haskell.org/ghc/ghc/-/issues/16901). For example with:

```ShellSession
hadrian/build -j --flavour=Quick stage2:lib:text
```

### Using simplified core in `.hi`

Build ghc with `hadrian/build -j --flavour=Quick+hi_core`.
Then run: `cabal --with-ghc=/srv/localhost/git/gitlab.haskell.org/ghc/ghc/_build/stage1/bin/ghc repl cabal-audit-hi`


## References documentation

### plugin
- GHC user-guide [Compiler Plugins](https://downloads.haskell.org/~ghc/9.6.2/docs/users_guide/extending_ghc.html#compiler-plugins)

### hie
- Haskell wiki [hie-files](https://gitlab.haskell.org/ghc/ghc-wiki-mirror/-/blob/master/hie-files.md)
- GHC module [GHC.Iface.Ext.Types](https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-Iface-Ext-Types.html)
- Weeder examples [repo](https://github.com/ocharles/weeder/blob/master/src/Weeder.hs)

### hi
- GHC module [GHC.IfaceToCore](https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-IfaceToCore.html)
