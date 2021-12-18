Lariat
======

**Lariat** is a project to define an abstract data type for lambda terms,
consisting of six basic operations: `app`, `abs`, `var`, `resolve`,
`destruct`, and `freevars`.

This repository presents the definition of these operations.  It also
contains implementations of this abstract data type (and possibly one
day variations on it) in various programming languages, including:

*   [Haskell](impl/Haskell/)

The version of the Lariat defined by this document is 0.1.  This
version number will be promoted to 1.0 once vetted sufficiently.

#### Table of Contents

*   [Background](#background)
*   [The Operations](#the-operations)
*   [Stepping Back](#stepping-back)
*   [Some Examples](#some-examples)

Background
----------

There are several approaches for representing lambda terms in software.

The naive approach is to represent them just as they are written on paper.  In this approach, whether
a variable, such as _x_, is free or bound depends on whether it is inside a lambda abstraction
λ _x_ or not.  If you need to manipulate it, you might need to rename it so that it doesn't conflict with
another variable also called _x_ that is bound to a different lambda abstraction.

This is tiresome and error-prone.  So other approaches were developed.

One such approach alternate is De Bruijn indexes, where variables are represented not by names,
but by numbers.  The number indicates which lambda abstraction the variable is bound to, if any;
a 1 indicates the immediately enclosing lambda abstraction, a 2 indicates the lambda abstraction
just above that, and so on.  If the number exceeds the number of enclosing lambda abstractions,
then it is a free variable.

But this, too, has some drawbacks, so people have devised a number of other approaches:

*   "nominal techniques" (Gabbay and Pitts)
*   "locally nameless" (various?)
*   "maps" (Sato et al.)
*   "bound" (Kmett)

among others.

But the point we would like to make is this:  At some level of abstraction _it does not matter_
which approach is chosen _as long as_ the approach satisfies the essential properties
that we require of lambda terms.

To this end, we present this abstract data type for lambda terms, which we
call **Lariat**, consisting of six operations.  The actual, concrete data structure
in which they are stored, and the actual, concrete mechanism by which names
become bound to terms, are of no consequence (and may well be hidden
from the programmer) so long as the implementation of the operations conforms
to the stated specification.

The Operations
--------------

### `var(n: name): term`

Given a name _n_, return a _free variable_ with the name _n_.

> **Note**: A free variable is a term; it can be passed to any operation
> that expects a term.

### `app(t: term, u: term): term`

Given a term _t_ and a term _u_, return an _application term_
which contains _t_ as its first subterm and _u_ as its second
subterm.

> **Note**: An application term is a term that is an
> ordered pair of terms.

### `abs(n: name, t: term): term`

Given a name _n_ and a term _t_, return an _abstraction term_
containing _t_', where _t_' is a version of _t_ where all free
variables named _n_ inside _t_' have been replaced with
bound variables.  These bound variables are bound to
the returned abstraction term.

> **Note**: a bound variable is a term, but the user cannot
> work with bound variables directly.  A bound variable is always
> bound to a particular abstraction term.  In the case of `abs`,
> the abstraction term to which variables are bound, is
> the term returned by the operation.

> **Note**: an abstraction term contains one subterm.  This
> subterm cannot be extracted directly, as it may contain bound
> variables, which the user cannot work with directly.

### `resolve(t: term, u: term): term`

Given a term _t_ and a term _u_, return either _t_ (if _t_ is
not an abstraction term) or _t_', where _t_' is a version of
_t_ where all bound variables in _t_' that were bound to _t_
itself have been replaced by _u_.

> **Note**: as stated above, a bound variable is always bound
> to an abstraction term.  The bound variables that are
> replaced by a `resolve` operation are always those that are
> bound to _t_.

> **Note**: this operation was specifically not named `subst`,
> as `subst` is often described as replacing *free* variables,
> while this operation replaces *bound* ones.  It was also
> specifically not named `beta` because it does not require
> that _t_ and _u_ come from the same application term.

### `destruct(t: term, f1: fun, f2: fun, f3: fun): X`

Given a term _t_ and three functions _f1_, _f2_, and _f3_
(each with a different signature, described below),
choose one of the three functions based on _t_, and evaluate it,
returning what it returns.

If _t_ is a free variable, evaluate _f1_(_n_) where _n_ is the name
of the free variable _t_.

If _t_ is an application term, evaluate _f2_(_u_, _v_) where _u_ is
the first subterm of _t_ and _v_ is the second subterm of _t_.

If _t_ is an abstraction term, evaluate _f3_(_t_).

> **Note**: the `destruct` operation's signature shown above was abbreviated to make
> it look less intimidating.  The full signature would be something more like
> 
>     destruct(t: term, f1: fun(n: name): X, f2: fun(u: term, v: term): X, f3: fun(t: term): X): X
> 

> **Note**: while _f1_ and _f2_ "take apart" the term they are working
> on, _f3_ does not, because the only operation that is allowed to
> "take apart" an abstraction term, is `resolve`, and calling
> `resolve` involves a choice (what to resolve the bound variable
> to) and `destruct` cannot make this choice for you.)

### `freevars(t: term): list(term)`

Given a term _t_, return a list of free variables contained in _t_.
These free variables may be located at any depth inside _t_, including
inside any and all abstraction terms contained in _t_.

Stepping Back
-------------

Now that we have given the operations, we can make some comments on them.

This is an abstract data type for lambda _terms_, not the lambda
_calculus_.  Naturally, one ought to be able to write a lambda calculus
normalizer using these operations (and this will be one of our goals in
the next section), but one is not restricted to that.  The terms
constructed using the Lariat operations may be used for any purpose
for which terms-with-name-binding might be useful.

It is `destruct` that allows us to examine a lambda term.  `destruct` is a
"destructorizer" in the sense described in the [Destructorizers][] article.
It is a slight variation on the conventional destructorizer pattern
described there, as it does not "take apart" abstraction terms, but it is
undoubtedly in the spirit of the thing.

It is regrettable that `freevars` is an intrinsic operation rather than
something that can be built as a recursive function that uses `destruct`,
but it seems it is needed in order to use `destruct` with generality,
for the following reasons.  The only practical way to "take apart" an abstraction
term is to `resolve` it with a known free variable.  But how do you
pick that free variable, so that it will not collide with any of the
free variables inside the term you are "taking apart"?  Short of
devising some clever namespacing for names, we need to know what
free variables are inside the term, _before_ `resolve`-ing it.
Thus `freevars` exists to fulfill that purpose.

Some Examples
-------------

### Example 1

As a warm-up, suppose we want to write a function that tells us if a
lambda term contains any free variable named `j`.

In this implementation and those that follow, we will assume we have
a simple functional language with the usual accoutrements (recursion,
`if`, `let` and so forth).

We'll also assume the existence of a value which is a
_name supply_; we call a function `pick` on it, and it returns
a name and new name supply.  Now, `pick` by itself does not ensure
the name is "fresh" (that is, not already used in a lambda term
that we're interested in), so we also inform `pick` of the set
of names that we don't want it to return.  In this particular case,
that set is the singleton set containing only `j`.

    --
    -- UNTESTED
    --
    let contains_j = fun(t, ns) ->
        destruct(r,
            fun(n) -> n == "j",
            fun(t, u) -> contains_j(t) || contains_j(u),
            fun(t) ->
                let
                    (n, ns') = pick(ns, ~{"j"})
                    t' = resolve(t, var(n))
                in
                    contains_j(t')
        )

### Example 2

What if we want to get the set of free variables present
in a lambda term?  We ought to be able to do this, and yet,
it's difficult unless we know of a name that we are certain
will not be among the names used by the free variables.
(Because in practice we "take apart" abstraction terms
by resolving them with a free variable.)

So for that task, we have `freevars` as one of the intrinsic
operations.

TODO: instead have an example of using `freevars` and
passing it to `pick` and walking down arbitrary abstraction
terms this way.

### Example 3

The next task is to write a beta-reducer.  We destruct
the term twice, once to ensure it is an application term,
and second to ensure the application term's first subterm
is an abstraction term.  Then we `resolve` the abstraction
with the application term's second subterm.

    --
    -- UNTESTED
    --
    let beta = fun(t) ->
        destruct(r,
            fun(n) -> var(n),
            fun(u, v) ->
                destruct(u,
                    fun(_) -> app(u, v),
                    fun(_, _) -> app(u, v),
                    fun(u) -> resolve(u, v)
                ),
            fun(t) -> t
        )

### Example 4 (untested)

The next task would be to search through a lambda term,
looking for a candidate application term to reduce, and
reducing it.

    --
    -- Returns [bool, term] where bool indicates "has rewritten"
    -- UNTESTED
    --
    let reduce = fun(t) ->
        if beta_reducible(t) then
            [true, beta(t)]
        else
            destruct(t,
                fun(n) -> [false, var(n)],
                fun(u, v) ->
                    let
                        r = reduce(u)
                    in
                        if r[0] then
                            [true, app(r[1], v)]
                        else
                            let
                                s = reduce(v)
                            in
                                [s[0], app(u, s[1])],
                fun(t) -> [false, t]
            )

From there it ought to be just a hop, a skip, and a jump
to a proper lambda term normalizer.

[Destructorizers]: http://github.com/cpressey/Destructorizers
