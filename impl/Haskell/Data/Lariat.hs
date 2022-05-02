module Data.Lariat (var, app, abs, destruct) where

import Prelude hiding (abs)

type Name = [String]

fresh :: [Name] -> Name
fresh names = expand $ longestName names []
    where
        expand [] = ["y"]
        expand (x:xs) = x:(x:xs)
        longestName [] acc = acc
        longestName (x:xs) acc = longestName xs (if (length x) > (length acc) then x else acc)

data Term α = FreeVar α
            | App (Term α) (Term α)
            | Abs (Term α)
            | BoundVar Integer
    deriving (Show, Ord, Eq)

var :: α -> Term α
var n = FreeVar n

app :: Term α -> Term α -> Term α
app t u = App t u

abs :: (Eq α) => α -> Term α -> Term α
abs n t = Abs (bind n t 0) where
    bind n (App t u) level = App (bind n t level) (bind n u level)
    bind n (Abs t) level = Abs (bind n t (level + 1))
    bind n t@(FreeVar m) level = if n == m then (BoundVar level) else t
    bind _ t _ = t

destruct :: Term α -> (α -> β) -> (Term α -> Term α -> β) -> (Term α -> α -> β) -> β
destruct (FreeVar n) f _ _ = f n
destruct (App t u)   _ f _ = f t u
destruct (Abs b)     _ _ f =
    let
        vars = freeVars b
        n = fresh vars
        u = unbind b (var n) 0
    in
        f u n
    where
        freeVars (FreeVar n) = [n]
        freeVars (App t u) = (freeVars t) ++ (freeVars u)
        freeVars (Abs b) = (freeVars b)
        freeVars _ = []

        unbind (App t u) x level = App (unbind t x level) (unbind u x level)
        unbind (Abs t) x level = Abs (unbind t x (level + 1))
        unbind t@(BoundVar i) x level = if i == level then x else t
        unbind t _ _ = t
