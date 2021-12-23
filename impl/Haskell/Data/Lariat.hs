module Data.Lariat (var, app, abs, resolve, destruct, freevars) where

import Prelude hiding (abs)
import Data.List (nub)

data Term α = FreeVar α
            | App (Term α) (Term α)
            | Abs (Term α)
            | BoundVar Integer
    deriving (Show, Ord, Eq)

var :: α -> Term α
var n = FreeVar n

app :: Term α -> Term α -> Term α
app t u = App t u

abs :: Eq α => α -> Term α -> Term α
abs n t = Abs (bind n t 0) where
    bind n (App t u) level = App (bind n t level) (bind n u level)
    bind n (Abs t) level = Abs (bind n t (level + 1))
    bind n t@(FreeVar m) level = if n == m then (BoundVar level) else t
    bind _ t _ = t

resolve :: Term α -> Term α -> Term α
resolve (Abs t) u = unbind t u 0 where
    unbind (App t u) x level = App (unbind t x level) (unbind u x level)
    unbind (Abs t) x level = Abs (unbind t x (level + 1))
    unbind t@(BoundVar i) x level = if i == level then x else t
    unbind t _ _ = t
resolve t u = t

destruct :: Term α -> (α -> β) -> (Term α -> Term α -> β) -> (Term α -> β) -> β
destruct (FreeVar n) f _ _ = f n
destruct (App t u)   _ f _ = f t u
destruct t@(Abs _)   _ _ f = f t

freevars :: Eq α => Term α -> [α]
freevars t = nub (freevars' t) where
    freevars' (FreeVar n) = [n]
    freevars' (App t u) = (freevars' t) ++ (freevars' u)
    freevars' (Abs t) = freevars' t
    freevars' _ = []
