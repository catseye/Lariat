module LariatDemo where

import Prelude hiding (abs)
import Data.Char (ord, chr)
import Data.List (intercalate)
import Data.Lariat (var, app, abs, destruct)

--
-- Test cases
-- (should really be unit tests; for now, run them manually)
--

n = ["n"]
m = ["m"]

showName n = intercalate "." n

testVar = (var n)
testAbs = (abs n (var n))
testApp = app testAbs testVar

testDestruct t = destruct t
                    (\n   -> "var:" ++ (showName n))
                    (\s t -> "app:" ++ (show s) ++ "," ++ (show t))
                    (\t n -> "abs:" ++ (showName n) ++ ":" ++ (show t))

test2a = testDestruct (var n)
test2b = testDestruct (app (var n) (var m))
test2c = testDestruct (abs n (var n))

--test3a = (freevars testApp)                     == ["n"]
--test3b = (freevars testAbs)                     == []
--test3c = (freevars (app (var "n") (var "n")))   == ["n"]

--test4a = (show $ resolve (testApp) (var "q"))               == "App (Abs (BoundVar 0)) (FreeVar \"n\")"
--test4b = (show $ resolve (abs "n" (var "m")) (var "q"))     == "FreeVar \"m\""
--test4c = (show $ resolve (abs "n" (var "n")) (var "q"))     == "FreeVar \"q\""
