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

cabal-audit lists the following declarations:

```ShellSession
CabalAudit.Test.Simple.afficheNombre: GHC.Base.pure, GHC.Show.show, System.IO.putStrLn, GHC.Tuple.Prim.()
CabalAudit.Test.Simple.maFonction: System.IO.putStr
```

## Roadmap

- [x] Collect external declarations.
- [ ] Analyze build dependencies.
- [ ] Implement the `cabal-audit` command.

## Note about ghc libraries

To get [ghc libraries](https://downloads.haskell.org/~ghc/9.6.2/docs/libraries/index.html) dependencies metadata:

### Using the plugin

Get the `libcabal-audit-plugin.so` path and use these arguments: `-fplugin-trustworthy -fplugin-library='${pluginSoPath};cabal-audit-plugin;CabalAudit.Plugin;[]'"`

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
