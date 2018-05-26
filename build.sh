#! /usr/bin/env nix-shell
#! nix-shell -i bash -p stack haskellPackages.brittany haskellPackages.hlint haskellPackages.stylish-haskell

set -e

cd hledger-to-influxdb

stylish-haskell -i src/Main.hs
brittany --write-mode inplace src/Main.hs
hlint src
stack build --nix
