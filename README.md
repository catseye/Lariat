Lariat
======

_Version 0.2_

**Lariat** is a project to define an abstract data type for lambda terms,
consisting of six basic operations: `equal` and `fresh` (on names), and
`app`, `abs`, `var`, and `destruct` (on terms).

This repository presents the definition of these operations.  It also
contains implementations of this abstract data type (and possibly one
day variations on it) in various programming languages, including:

*   [Haskell](impl/Haskell/)

The version of the Lariat defined by this document is 0.2.  This
version number will be promoted to 1.0 once vetted sufficiently.

#### Table of Contents

*   [Background](#background)
*   [The Operations](#the-operations)
*   [Some Examples](#some-examples)
*   [Discussion](#discussion)

Background
----------

There are several approaches for representing lambda terms in software.

The naive approach is to represent them just as they are written on paper.  In this approach, whether
a variable, such as _x_, is free or bound depends on whether it is inside a lambda abstraction
λ _x_ or not.  If you need to manipulate it (or the abstraction it's bound to), you might need
to rename it so that it doesn't conflict with another variable also called _x_ that is bound
to a different lambda abstraction.

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

Names
-----

In any explication of name binding we must deal with names.  In Lariat 0.1, names
were left almost entirely undefined; the only operation they were required to
support was comparison of two names for equality.  While this extreme level of
abstraction might be attractive from a theoretical perspective, it introduced
complications and awkwardness into practical use of the abstract data type.

In Lariat 0.2, names are treated as abstract objects much like terms, and we
specify that names must support the following two operations:

### `equal(n: name, m: name): boolean`

Given a name _n_ and a name _m_, return true if they are identical names,
otherwise return false.

### `fresh(ns: set of name): name`

Given a set of names _ns_, return a name which does not occur in _ns_.
We say this returned name is "fresh for _ns_".

The means by which the fresh name is generated is abstracted away; we
only care about the guarantee that it is not a member of _ns_.  For
discussion on implementation, see [Appendix A](#appendix-a).

The operation should be deterministic in the sense that, given the
same set of names, it should always return the same fresh name.

> **Note**: It is not required that the `fresh` operation be
> exposed to the user; it is, rather, a structural requirement
> of a correct implementation of `destruct`, below.

> **Note**: Beyond these two operations, it would be not unexpected that an
> implementation of Lariat would provide other operations such as constructing
> a new name from a textual representation, rendering a given name to
> a canonical textual representation, and suchlike.  From the perspective
> of Lariat itself these are ancillary operations, and as such will not be
> defined in this document.

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

> **Note**: An application term is a term that behaves just as an
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
> the term returned by the `abs` operation.

> **Note**: an abstraction term contains one subterm.  This
> subterm cannot be extracted directly, as it may contain bound
> variables, which the user cannot work with directly.

### `destruct(t: term, f1: fun, f2: fun, f3: fun): X`

Given a term _t_ and three functions _f1_, _f2_, and _f3_
(each with a different signature, described below),
choose one of the three functions based on _t_, and evaluate it,
returning what it returns.

If _t_ is a free variable, evaluate _f1_(_n_) where _n_ is the name
of the free variable _t_.

If _t_ is an application term, evaluate _f2_(_u_, _v_) where _u_ is
the first subterm of _t_ and _v_ is the second subterm of _t_.

If _t_ is an abstraction term, evaluate _f3_(_u_, _n_) where _u_ is
a version of _t_ where all bound variables in _u_ that were bound to
_u_ itself have been replaced by _n_, where _n_ is a fresh name
(i.e. a name that does not occur free anywhere in _u_).

> **Note**: as stated above, a bound variable is always bound
> to an abstraction term.  The bound variables that are replaced by the
> `destruct` of an abstraction term are always and only those that are
> bound to the abstraction term being `destruct`ed.

> **Note**: the `destruct` operation's signature shown above was abbreviated to make
> it look less intimidating.  The full signature would be something more like
> 
>     destruct(t: term, f1: fun(n: name): X, f2: fun(u: term, v: term): X, f3: fun(u: term, n: name): X): X
> 

Some Examples
-------------

We will now give some concrete examples of how these operations
can be used, but first, we would like to emphasize that
this is an abstract data type for lambda _terms_, not the lambda
_calculus_.  Naturally, one ought to be able to write a lambda calculus
normalizer using these operations (and this will be one of our goals in
the next section), but one is not restricted to that activity.  The terms
constructed using the Lariat operations may be used for any purpose
for which terms-with-name-binding might be useful.

### Example 1

A common task is to obtain the set of free variables present
in a lambda term.  This is not difficult; we only need to
keep track of the new free variables we introduce ourselves
when we `destruct` an abstraction term, and make sure not to
include any of them when we report the free variables we found.

    let freevars = fun(t, ours) ->
        destruct(t,
            fun(n) -> if n in ours then {} else {n},
            fun(u, v) -> freevars(u, ours) + freevars(v, ours),
            fun(u, n) -> freevars(u, ours + {n})
        )

### Example 2

Given an abstraction term and a value, return a version of
the body of the abstraction term where every instance of the
variable bound to the abstraction term is replaced by the given
value.  We can call this operation `resolve`.

    let resolve = fun(t, x) ->
        destruct(t,
            fun(n) -> t,
            fun(u, v) -> t,
            fun(u, n) -> replace_all(u, n, x)
        )
    where replace_all = fun(t, m, x) ->
        destruct(t,
            fun(n) -> if n == m then x else var(n),
            fun(u, v) -> app(replace_all(u, m, x), replace_all(v, m, x))
            fun(u, n) -> abs(n, replace_all(u, m, x))
        )

Note that this operation was specifically *not* called `subst`,
because the name `subst` is often given to a process that
replaces *free* variables, while this operation replaces
*bound* ones.  It was also specifically not named `beta`
because it does not require that _t_ and _x_ come from the same
application term.

### Example 3

The next task is to write a beta-reducer.  We destruct
the term twice, once to ensure it is an application term,
and second to ensure the application term's first subterm
is an abstraction term.  Then we use `resolve`, above, to
plug the application term's second subterm into the
abstraction term.

    let beta = fun(r) ->
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

In fact, we could merge this implementation with the
implementation of `resolve` and this would save a call
to `destruct`.

### Example 4

The next task would be to search through a lambda term,
looking for a candidate application term to reduce, and
reducing it.

    --
    -- Returns [bool, term] where bool indicates "has rewritten"
    --
    let reduce = fun(t) ->
        if is_beta_reducible(t) then
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
                                if s[0] then
                                    [true, app(u, s[1])]
                                else
                                    [false, app(r[1], s[1])],
                fun(t) -> [false, t]
            )

From there it ought to be just a hop, a skip, and a jump
to a proper lambda term normalizer:

    let normalize(t) ->
        let
            r = reduce(t)
        in
            if r[0] then
                normalize(r[1])
            else
                t

Discussion
----------

`var`, `app`, and `abs` construct terms, while `destruct` takes them apart.
Constructing terms is the easy part; it's taking them apart properly that's
hard.

`destruct` is a "destructorizer" in the sense described in 
[this article on Destructorizers](http://github.com/cpressey/Destructorizers).
In fact, this use case of "taking apart" lambda terms was one
of the major motivations for formulating the destructorizer
concept.

When working with lambda terms, one is often concerned with
comparing two lambda terms for equality, modulo renaming of bound
variables.  We haven't introduced such an operation because it should
be possible to build such an operation using `destruct`; basically,
render the two terms as text (or some other concrete representation),
then compare the texts for equality.

But of course such an operation could be provided as a native
operation for performance or convenience.  Similarly, although
we have shown that we can implement `freevars` using the operations
of the abstract data type, it is expected that it would already
be implemented in the implementation of the abstract data type
(to correctly implement `destruct`), so could be exposed to the
user as well.

Appendices
----------

### Appendix A

#### On the implementation of names.

One way to implement names as defined in Lariat 0.2 is to use _qualified names_.
A qualified name is an ordered list of _name segments_, where each name segment
is what a name was in Lariat 0.1, i.e. the only operation we require name segments
to support is comparison of two name segments for equality.  (Comparing two
qualified names for equality is straightforwardly derived from this.)

The client of the Lariat ADT is not, by themselves, required to qualify any
names; if each qualified name they supply in their usage consists of only a
single name segment, that's fine.

However, qualified names permit the definition of a simple algorithm for
generating a fresh name, i.e. a name which does not appear in a given set
of names.  To wit,

*   pick the longest qualified name from the set;
*   prepend an arbitrary name segment to that qualified name.

If there are more than one longest names in the set, pick one arbitrarily.

In addition, a simple way to pick an arbitrary name segment to prepend to
it, is to look at the leftmost name segment already in the qualified name.

So we can assume an algorithm like this is in use.  But ultimately, any
implementation which satisfies the two operations required of names
(`equal` and `fresh`) is acceptable.
