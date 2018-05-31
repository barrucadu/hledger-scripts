#! /usr/bin/env nix-shell
#! nix-shell -i bash -p stack haskellPackages.brittany haskellPackages.hlint haskellPackages.stylish-haskell

set -e

cd hledger-to-influxdb
find . -name '*.hs' -exec stylish-haskell -i {} \;
find . -name '*.hs' -exec brittany --write-mode inplace {} \;
hlint src
stack build --nix
