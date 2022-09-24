#!/bin/sh

MISSING=""
if command -v ghc > /dev/null 2>&1; then
    echo "testing with ghc"
    (cd impl/Haskell && ghc Main.hs -e main) || exit 1
else
    MISSING="${MISSING}G"
fi
if command -v runhugs > /dev/null 2>&1; then
    echo "testing with hugs"
    (cd impl/Haskell && runhugs Main.hs) || exit 1
else
    MISSING="${MISSING}H"
fi
if [ "x${MISSING}" = "xGH" ]; then
    echo "Neither ghci nor runhugs found on executable search path. Aborting."
    exit 1
fi
