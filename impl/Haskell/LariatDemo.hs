module LariatDemo where

import Prelude hiding (abs)
import Data.Char (ord, chr)
import Data.List (intercalate, nub)
import Data.Lariat (var, app, abs, destruct)

--
-- Test cases
-- (should really be unit tests; for now, run them manually)
--

n = ["n"]
m = ["m"]

testVar = (var n)
testAbs = (abs n (var n))
testApp = app testAbs testVar

testDestruct t = destruct t
                    (\n   -> "var:" ++ (show n))
                    (\u v -> "app:" ++ (show u) ++ "," ++ (show v))
                    (\u n -> "abs:" ++ (show n) ++ ":" ++ (show u))

test2a = testDestruct (var n)
test2b = testDestruct (app (var n) (var m))
test2c = testDestruct (abs n (var n))

freevars t = nub $ freevars' t [] where
    freevars' t ours = destruct t
                        (\n   -> if elem n ours then [] else [n])
                        (\u v -> freevars' u ours ++ freevars' v ours)
                        (\u n -> freevars' u (n:ours))

test3a = (freevars testApp)               == [n]
test3b = (freevars testAbs)               == []
test3c = (freevars (app (var n) (var n))) == [n]

--test4a = (show $ resolve (testApp) (var "q"))               == "App (Abs (BoundVar 0)) (FreeVar \"n\")"
--test4b = (show $ resolve (abs "n" (var "m")) (var "q"))     == "FreeVar \"m\""
--test4c = (show $ resolve (abs "n" (var "n")) (var "q"))     == "FreeVar \"q\""
