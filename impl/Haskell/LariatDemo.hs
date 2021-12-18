module LariatDemo where

import Prelude hiding (abs)
import Data.Char (ord, chr)
import Data.Lariat (var, app, abs, resolve, destruct, freevars)

--
-- Test cases
-- (should really be unit tests; for now, run them manually)
--

test1 = app (abs "n" (var "n")) (var "n")

testDestruct t = destruct t
                    (\n   -> "var:" ++ (show n))
                    (\s t -> "app:" ++ (show s) ++ "," ++ (show t))
                    (\t   -> "abs:" ++ (show t))

test2a = testDestruct (var "n")
test2b = testDestruct (app (var "n") (var "m"))
test2c = testDestruct (abs "n" (var "n"))

test3a = freevars test1
test3b = freevars (app (var "n") (var "n"))

test4a = resolve (test1) (var "q")  -- test1 is not an abs so no change
test4b = resolve (abs "n" (var "m")) (var "q")
test4c = resolve (abs "n" (var "n")) (var "q")

--
-- Name supply
--

intToName n =
    let
        p = if n > 26 then (intToName (n `div` 26)) else ""
        n' = n `mod` 26
        c = chr (ord 'a' + n')
    in
        p ++ [c]

names = map (intToName) [0..]

pick exclude names =
    let
        [n] = take 1 names
        names' = drop 1 names
        excluded = elem n exclude
    in
        if excluded then pick exclude names' else (n, names')
