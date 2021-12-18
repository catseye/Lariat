module LariatDemo where

import Prelude hiding (abs)
import Data.Lariat (var, app, abs, resolve, destruct, freevars)

test = app (abs "n" (var "n")) (var "n")