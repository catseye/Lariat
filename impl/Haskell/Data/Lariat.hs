module Data.Lariat (var, app, abs, resolve, destruct, freevars) where

import Prelude hiding (abs)

data Term α = FreeVar α
            | App (Term α) (Term α)
            | Abs α (Term α)
            | BoundVar Integer
    deriving (Show, Ord, Eq)


var :: α -> Term α
var n = FreeVar n

app :: Term α -> Term α -> Term α
app t u = App t u

abs :: α -> Term α -> Term α
abs n t = Abs n t  -- FIXME this is very wrong

resolve :: Term α -> Term α -> Term α
resolve t u = error "NotImplemented"

destruct :: Term α -> (Term α -> τ) -> (Term α -> Term α -> τ) -> (Term α -> τ) -> τ
destruct t f1 f2 f3 = error "NotImplemented"

freevars :: Term α -> [α]
freevars t = []
