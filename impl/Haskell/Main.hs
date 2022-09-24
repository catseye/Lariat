module Main where

--
-- Test suite for Haskell implementation of Lariat.
-- This "Main" module just runs the tests and exits.
--

import Prelude hiding (abs)
import Data.Char (ord, chr)
import Data.List (nub)
import Data.Lariat (name, var, app, abs, destruct)

--
-- Test helpers
--

-- First, a simple function to express our illustrative tests:
-- Given a list of pairs, show those pairs that are not equal.
-- Anything other than an empty list returned indicates a mistake.

expect [] = []
expect ((a, b):rest) = if (show a) == b then expect rest else ((a, b):expect rest)

--
-- Convenience function to destruct Either
--

right (Right x) = x

--
-- Fixtures
--

n = name "n"
m = name "m"
q = name "q"

demoVar = (var n)
demoAbs = (abs n (var n))
demoApp = app demoAbs demoVar
demoTerm1 = (app (app (abs q (var q)) (abs q (var q))) (var n))
demoTerm2 = (app (var n) (app (abs q (var q)) (var m)))

--
-- Test cases
--

showDestruct t = destruct t
                    (\n   -> "var:" ++ (show n))
                    (\u v -> "app:" ++ (show u) ++ "," ++ (show v))
                    (\u n -> "abs:" ++ (show n) ++ ":" ++ (show u))

testShowDestruct = expect
    [
        (showDestruct (var n),               "\"var:n\""),
        (showDestruct (app (var n) (var m)), "\"app:FreeVar n,FreeVar m\""),
        (showDestruct (abs n (var n)),       "\"abs:y:FreeVar y\"")
    ]


freevars t = nub $ freevars' t [] where
    freevars' t ours = destruct t
                        (\n   -> if elem n ours then [] else [n])
                        (\u v -> freevars' u ours ++ freevars' v ours)
                        (\u n -> freevars' u (n:ours))

testFreeVars = expect
    [
        (freevars demoApp,                 "[n]"),
        (freevars demoAbs,                 "[]"),
        (freevars (app (var n) (var n)),   "[n]")
    ]


resolve t x = destruct t
                (\n   -> t)
                (\u v -> t)
                (\u n -> replaceAll u n x)

replaceAll t m x = destruct t
                    (\n   -> if n == m then x else (var n))
                    (\u v -> app (replaceAll u m x) (replaceAll v m x))
                    (\u n -> abs n (replaceAll u m x))

testResolve = expect
    [
        (resolve (demoApp) (var q),       "App (Abs (BoundVar 0)) (FreeVar n)"),
        (resolve (abs n (var m)) (var q), "FreeVar m"),
        (resolve (abs n (var n)) (var q), "FreeVar q")
    ]


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

testBeta = expect
    [
        (beta demoApp,                 "FreeVar n"),
        (beta demoAbs,                 "Abs (BoundVar 0)"),
        (beta (var q),                 "FreeVar q")
    ]


isBetaReducible t = destruct t
    (\_   -> False)
    (\u v -> destruct u
        (\_   -> False)
        (\_ _ -> False)
        (\_ _ -> True)
    )
    (\_ _ -> False)

testIsBetaReducible = expect
    [
        (isBetaReducible demoApp, "True"),
        (isBetaReducible demoAbs, "False"),
        (isBetaReducible (var q), "False")
    ]


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
        (\u n -> Left (abs n u))

testReduceOnce = expect
    [
        (reduceOnce demoApp,           "Right (FreeVar n)"),
        (reduceOnce demoAbs,           "Left (Abs (BoundVar 0))"),
        (reduceOnce (var q),           "Left (FreeVar q)"),
        (reduceOnce demoTerm1,         "Right (App (Abs (BoundVar 0)) (FreeVar n))"),
        (reduceOnce (right $ reduceOnce demoTerm1), "Right (FreeVar n)"),
        (reduceOnce demoTerm2,         "Right (App (FreeVar n) (FreeVar m))")
    ]


allTests :: String
allTests =
    -- Hugs can only Show tuples up to 5-tuples.  Not 6-tuples.  Thus this nesting:
    case ((testShowDestruct, testFreeVars, testResolve, testBeta, testIsBetaReducible), testReduceOnce) of
        (([],[],[],[],[]), []) -> "ok"
        other -> error (show other)

main = do
    let x = allTests
    putStrLn x
