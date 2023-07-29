# cabal-audit

cabal-audit is a command line tool to list the external declarations used in a given library.
The goal is to inspect the build dependencies with the
[security-advisories](https://github.com/haskell/security-advisories).

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

## Features

- [x] List external declaration.
- [ ] Handle type class instance evidences.
- [ ] Figure how to get all the hie files (for ghc and hackage libs).
- [ ] Discover the list of exposed module.
