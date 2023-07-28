# cabal-audit

cabal-audit is a command line tool to list the external declarations used in a given library.
The goal is to inspect the build dependencies with the
[security-advisories](https://github.com/haskell/security-advisories).

## Features

- [x] List external declaration.
- [ ] Handle type class instance evidences.
- [ ] Figure how to get all the hie files (for ghc and hackage libs).
- [ ] Discover the list of exposed module.
