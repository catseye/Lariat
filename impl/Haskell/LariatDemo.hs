module LariatDemo where

import Prelude hiding (abs)
import Data.Char (ord, chr)
import Data.Lariat (var, app, abs, resolve, destruct, freevars)

--
-- Test cases
-- (should really be unit tests; for now, run them manually)
--

testVar = (var "n")
testAbs = (abs "n" (var "n"))
testApp = app testAbs testVar

testDestruct t = destruct t
                    (\n   -> "var:" ++ (show n))
                    (\s t -> "app:" ++ (show s) ++ "," ++ (show t))
                    (\t   -> "abs:" ++ (show t))

test2a = testDestruct (var "n")
test2b = testDestruct (app (var "n") (var "m"))
test2c = testDestruct (abs "n" (var "n"))

test3a = (freevars testApp)                     == ["n"]
test3b = (freevars testAbs)                     == []
test3c = (freevars (app (var "n") (var "n")))   == ["n"]

test4a = (show $ resolve (testApp) (var "q"))               == "App (Abs (BoundVar 0)) (FreeVar \"n\")"
test4b = (show $ resolve (abs "n" (var "m")) (var "q"))     == "FreeVar \"m\""
test4c = (show $ resolve (abs "n" (var "n")) (var "q"))     == "FreeVar \"q\""

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

--
-- "Contains a free variable named _j_" example
--

contains j t names =
    destruct t
        (\n   -> n == j)
        (\u v -> contains j u names || contains j v names)
        (\u   ->
            let
                (n, names') = pick [j] names
                t' = resolve t (var n)
            in
                contains j t' names'
        )

testContains1 = (contains "n" testApp names)                 == True
testContains2 = (contains "m" testApp names)                 == False
testContains3 = (contains "n" (abs "n" (var "n")) names)     == False

--
-- "beta-reduce a term" example
--

beta t = destruct t
            (\n   -> var n)
            (\u v -> destruct u
                (\_   -> app u v)
                (\_ _ -> app u v)
                (\u   -> resolve u v)
            )
            (\u   -> u)

testBeta1 = (show $ beta testApp)                    == "FreeVar \"n\""
testBeta2 = (show $ beta testAbs)                    == "Abs (BoundVar 0)"
