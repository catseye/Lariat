#!/bin/sh

(cd impl/Haskell && ghc Main.hs -e main) || exit 1

