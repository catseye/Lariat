#!/bin/sh

(cd impl/Haskell && ghc LariatDemo.hs -e allTests) || exit 1

