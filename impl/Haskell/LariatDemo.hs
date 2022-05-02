module LariatDemo where

import Prelude hiding (abs)
import Data.Char (ord, chr)
import Data.List (intercalate, nub)
import Data.Lariat (name, var, app, abs, destruct)

--
-- Test cases
-- (should really be unit tests; for now, run them manually)
--

n = name "n"
m = name "m"
q = name "q"

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

resolve t x = destruct t
                (\n   -> t)
                (\u v -> t)
                (\u n -> replaceAll u n x)

replaceAll t m x = destruct t
                    (\n   -> if n == m then x else (var n))
                    (\u v -> app (replaceAll u m x) (replaceAll v m x))
                    (\u n -> abs n (replaceAll u m x))

test4a = (show $ resolve (testApp) (var q))           == "App (Abs (BoundVar 0)) (FreeVar n)"
test4b = (show $ resolve (abs n (var m)) (var q))     == "FreeVar m"
test4c = (show $ resolve (abs n (var n)) (var q))     == "FreeVar q"

--
-- "beta-reduce a term" example
--

beta t = destruct t
    (\n   -> t)
    (\u v -> destruct u
        (\_   -> app u v)
        (\_ _ -> app u v)
        (\u n -> replaceAll u n v)
    )
    (\_ _ -> t)

testBeta1 = (show $ beta testApp)                    == "FreeVar n"
testBeta2 = (show $ beta testAbs)                    == "Abs (BoundVar 0)"
testBeta3 = (show $ beta (var q))                    == "FreeVar q"

isBetaReducible t = destruct t
    (\_   -> False)
    (\u v -> destruct u
        (\_   -> False)
        (\_ _ -> False)
        (\_ _ -> True)
    )
    (\_ _ -> False)

testIsBeta1 = (isBetaReducible testApp)       == True
testIsBeta2 = (isBetaReducible testAbs)       == False
testIsBeta3 = (isBetaReducible (var q))       == False

--
-- TODO NEEDS WORK
--

reduceOnce t =
    if isBetaReducible t then (Right (beta t)) else destruct t
        (\n   -> Left (var n))
        (\u v ->
            case reduceOnce u of
                Right r -> Right (app r v)
                Left f  ->
                    case reduceOnce v of
                        Right s -> Right (app u s)
                        Left  g -> Left (app f g)
        )
        (\u _ -> Left u)

testReduceOnce1 = (show $ reduceOnce testApp)          == "Right (FreeVar n)"
testReduceOnce2 = (show $ reduceOnce testAbs)          == "Left (Abs (BoundVar 0))" -- ???
testReduceOnce3 = (show $ reduceOnce (var q))          == "Left (FreeVar q)"

testReduceOnce4 =
    let
       t = (app (app (abs q (var q)) (abs q (var q))) (var n))
    in
       (show $ reduceOnce t) == "Right (App (Abs (BoundVar 0)) (FreeVar n))"

testReduceOnce5 =
    let
       t = (app (app (abs q (var q)) (abs q (var q))) (var n))
       (Right t') = reduceOnce t
    in
       (show $ reduceOnce t') == "Right (FreeVar n)"

testReduceOnce6 =
    let
       t = (app (var n) (app (abs q (var q)) (var m)))
    in
       (show $ reduceOnce t) == "Right (App (FreeVar n) (FreeVar m))"
