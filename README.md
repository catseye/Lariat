Lariat
======

**Lariat** is a project to define an abstract data type for lambda terms,
consisting of six basic operations: `app`, `abs`, `var`, `resolve`,
`destruct`, and `fv`.

This repository, first and foremost, presents the definitions of these
operations.  Secondarily, it may (one day) contain implementations of
this abstract data type (and possibly variations on it) in different
programming languages.

The version of the Lariat abstract data type defined by this document
is version 0.1.  To be promoted to 1.0 once vetted sufficiently.

#### Table of Contents

*   [Background](#background)
*   [The Operations](#the-operations)
*   [Some Examples](#some-examples)
*   [Discussion](#discussion)

Background
----------

There are several approaches for representing lambda terms in software.

The naive approach is to represent them just as they are written on paper.  In this approach, if you see
a variable, such as _x_, whether _x_ is free or bound depends on whether it is inside a lambda abstraction
λ _x_ or not.  If you need to manipulate it, you might need to rename it so that it doesn't conflict with
another variable also called _x_ that is bound to a different lambda abstraction.

This is tiresome and error-prone.  So other approaches were developed.

One such approach is called De Bruijn indexes.  In this approach, variables are represented not by names,
but by numbers.  The number indicates which lambda abstraction the variable is bound to, if any.  That is,
a 1 indicates it is bound to the immediately enclosing lambda abstraction, a 2 indicates it is bound to
the lambda abstraction just above that, and so on.  If the number exceeds the number of lambda abstractions
which enclose the variable, it is a free variable.

This, too, has some drawbacks, so people have devised a number of other approaches:

*   "nominal techniques" (Gabbay and Pitts)
*   "locally nameless" (various?)
*   "maps" (Sato et al.)
*   "bound" (Kmett)

and so forth.

But the point is, at some level of abstraction _it does not matter_ which
approach is chosen _as long as_ the approach satisfies the essential properties
that we require lambda terms to have.

To this end we present this abstract data type for lambda terms, which we
call "Lariat", consisting of six operations.  The actual, concrete data type
by which they are stored, and the actual, concrete mechanism by which names
become bound to terms, are of no real consequence (and may well be hidden
from the programmer) so long as the implementation of the operations conforms
to their stated specification.

The Operations
--------------

### `var(n: name): term`

Given a name _n_, return a _free variable_ with the name _n_.

(Note: A free variable is a term; it can be passed to any operation
that expects a term.)

### `app(t1: term, t2: term): term`

Given a term _t1_ and a term _t2_, return an _application term_
which contains _t1_ as its head and _t2_ as its tail.

### `abs(n: name, t: term): term`

Given a name _n_ and a term _t_, return an _abstraction term_
containing _t'_, where _t'_ is a version of _t_ where all free
variables named _n_ inside _t'_ have been replaced with
bound variables.  These bound variables are bound to
the returned abstraction term.

(Note: a bound variable is a term, but the user cannot
work with them directly.  A bound variable is always
bound to an abstraction term.  In the case of `abs`,
the abstraction term to which variables are bound, is
the term returned by the operation.)

### `resolve(t1: term, t2: term): term`

Given a term _t1_ and a term _t2_, return either _t1_ (if _t1_ is
not an abstraction term) or _t1'_, where _t1'_ is a version of
_t1_ where all bound variables in _t1'_ that were bound to _t1_
itself have been replaced by _t2_.

(Note: as states above, a bound variable is always bound
to an abstraction term.  The bound variables that are
replaced by a `resolve` operation are always those that are
bound to _t1_.)

(Note: this operation was specifically not named `subst`,
as `subst` is often described as replacing *free* variables,
while this replaces bound ones.  It was also specifically not
named `beta` or similar because it does not require _t1_ and _t2_
to come from the same application term.)

### `destruct(t: term, f1: fun, f2: fun, f3: fun): X`

Given a term _t_ and three functions _f1_, _f2_, and _f3_
(each with a different signature, described below),
choose one of the three functions based on _t_, and evaluate it,
returning what it returns.

If _t_ is a free variable, evaluate _f1_(_n_) where _n_ is the name
of the free variable _t_.

If _t_ is an application term, evaluate _f2_(t1, t2) where _t1_ is
the head of _t_ and _t2_ is the tail of _t_.

If _t_ is an abstraction term, evaluate _f3_(_t_).

(Note: the `destruct` operation's signature was abbreviated above to make
it look less intimidating.  The full signature would be something like
`destruct(t: term, f1: fun(n: name): X, f2: fun(t1: term, t2: term): X, f3: fun(t: term): X): X`.)

(Note: `destruct` is a [destructorizer][] in the sense described
in the linked article.  It is a slight variation on the conventional
destructorizer pattern, but it is undoubtedly in the spirit of
the thing.)

(Note: while _f1_ and _f2_ "take apart" the term they are working
on, _f3_ does not, because the only operation that is allowed to
"take apart" an abstraction term, is `resolve`, and calling
`resolve` involves a choice (what to resolve the bound variable
to) and `destruct` cannot make this choice for you.)

### `fv(t: term): list(term)`

Given a term _t_, return a list of free variables contained in _t_.
These free variables may be located at any depth inside _t_, including
inside any and all abstraction terms contained in _t_.

(Note: I wish this operation was not required, but it seems it is
needed in order to do many practical things with `destruct`.  One
would hope the list of free variables could be obtained by usage of
`destruct` inside a recursive function, at it is simply a tree walk
of the term.  But the only practical way to "take apart" an abstraction
term is to `resolve` it will a known free variable.  But how do you
pick that free variable, so that it will not collide with any of the
free variables inside the term you are "taking apart"?  Short of
devising some clever namespacing for names, we need to know what
free variables are insie the term first, _before_ `resolve`-ing it.
Thus `fv` exists to fulfil that purpose.)

Some Examples
-------------

We'll now consider some examples.  But first, take note.
This is an abstract data type for lambda _terms_, not the lambda
_calculus_.  One can use these lambda terms for any purpose they
like.  But naturally one ought to be able to write a lambda calculus
evaluator (i.e. a reducer to normal form) using these operations.
So, that will be our ultimate goal in this section.

It is `destruct` that allows us to examine a lambda term.

Suppose we want to write a function that tells us if a
lambda term contains any free variable named `j`.

In this implementation, we assume we have a value which is a
_name supply_; we call a function `pick` on it, and it returns
a name and new name supply.  Now, `pick` by itself does not ensure
the name is "fresh" (that is, not already used in a lambda term
that we're interested in), so we also inform `pick` of the set
of names that we don't want it to return.  In this case that set is `j`.

    let contains_j = fun(t, ns) ->
        destruct(r,
            fun(n) -> n == "j",
            fun(t1, t2) -> contains_j(t1) || contains_j(t2),
            fun(t) ->
                let
                    (n, ns') = pick(ns, ~{"j"})
                    t' = resolve(t, var(n))
                in
                    contains_j(t')
        )

Next: What if we want to get the set of free variables present
in a lambda term?  We ought to be able to do this, and yet,
it's difficult unless we know of a name that we are certain
will not be among the names used by the free variables.
(Because in practice we "take apart" abstraction terms
by resolving them with a free variable.)

So for that task, we have `fv`.

The next task is to write a beta-reducer.  We destruct
the term twice, once to ensure it is an application term,
and second to ensure the application term's head is an
abstraction term.  Then we `resolve` the abstraction
with the application term's tail.

    let beta = fun(t) ->
        destruct(r,
            fun(n) -> var(n),
            fun(t1, t2) ->
                destruct(t1,
                    fun(_) -> app(t1, t2),
                    fun(_, _) -> app(t1, t2),
                    fun(t) -> resolve(t, t2)
                ),
            fun(t) -> t
        )

The next task would be to search through a lambda term,
looking for a candidate application term to beta-reduce.
This doesn't sound hard.  But is still not yet written

Discussion
----------

There is nothing preventing an abstract data type which is a
proper superclass of this abstract data type, adding more operations.
However, many operations one might like to perform on these lambda
terms (such as reducing lambda terms to normal form, for instance)
ought to be expressible using `destruct`, possibly recursively.

[destructorizer]: http://github.com/catseye/Destructorizers
