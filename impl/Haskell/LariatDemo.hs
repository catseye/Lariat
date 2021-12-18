module LariatDemo where

import Prelude hiding (abs)
import Data.Lariat (var, app, abs, resolve, destruct, freevars)

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
