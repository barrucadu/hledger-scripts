#! /usr/bin/env nix-shell
#! nix-shell -i bash -p stack haskellPackages.brittany haskellPackages.hlint haskellPackages.stylish-haskell

set -e

# first style + lint
find . -name '*.hs' -exec stylish-haskell -i {} \;
find . -name '*.hs' -exec brittany --write-mode inplace {} \;
hlint .

# then build
for dir in *; do
  if [[ -d "$dir" ]] && [[ -e "$dir/stack.yaml" ]]; then
    pushd $dir
    stack build --nix
    popd
  fi
done
