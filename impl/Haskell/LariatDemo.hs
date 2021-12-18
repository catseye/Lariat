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
