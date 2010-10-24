Installing Loker
================

Dependencies
------------

### Compiler ###

[GHC][ghc] 6.10 or newer is recommended. It can be installed either from your
distro's packages, or by downloading and installing a binary release.

### Library dependencies ###

Library dependencies can be installed from your distro's packages or via
cabal-install (see below). Also you might have some of them already if you have
installed the Haskell platform.

* Parsec 3. **The version is essential, you won't be able to build
  Loker using Parsec 2**
* mtl

Installing using cabal-install
------------------------------

If you have [cabal-install][cabal-install], just go to the Loker root directory
and type

    cabal install

This will:

1. Install all necessary library dependencies (won't install ghc, though).
   Note that they will be installed ignoring your system's package
   manager. If this is undesirable, install all the dependencies using your
   package manager before executing `cabal install`.
2. Build and compile locker, and install executables in `$HOME/.cabal/bin`.

[ghc]: http://haskell.org/ghc/
[cabal-install]: http://www.haskell.org/haskellwiki/Cabal-Install

Installing using Setup.hs
----------------------

Note that this won't automatically install dependencies.

Execute the following commands in Loker root directory

    runhaskell Setup.hs configure --user --prefix="$HOME"
    runhaskell Setup.hs build
    runhaskell Setup.hs install --user

This will install executables in `$HOME/bin`. (You can specify another prefix as
well.)
