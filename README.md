Lariat
======

_Version 0.3_

**Lariat** is a project to define a total abstract data type for
proper lambda terms, consisting of four basic operations:
`app`, `abs`, `var`, and `destruct`.

This repository presents the definition of these operations.  It also
contains implementations of this abstract data type in various
programming languages, currently including:

*   [Haskell](impl/Haskell/)

The version of the Lariat defined by this document is 0.3.  This
version number will be promoted to 1.0 once vetted sufficiently.

#### Table of Contents

*   [Background](#background)
*   [The Operations](#the-operations)
*   [Some Examples](#some-examples)
*   [Discussion](#discussion)

Background
----------

There are several approaches to representing lambda terms in software.

The naive approach is to represent them just as they are written on paper.  In this approach, whether
a variable, such as _x_, is free or bound depends on whether it is inside a lambda abstraction
λ _x_ or not.  If you need to manipulate it (or the abstraction it's bound to), you might need
to rename it so that it doesn't conflict with another variable also called _x_ that is perhaps
free or perhaps bound to a different lambda abstraction.

This is tiresome and error-prone.  So other approaches were developed.

One such alternate approach is [De Bruijn indexes](https://en.wikipedia.org/wiki/De_Bruijn_index) (Wikipedia),
where variables are represented not by names, but by numbers.
The number indicates which lambda abstraction the variable is bound to, if any;
a 1 indicates the immediately enclosing lambda abstraction, a 2 indicates the lambda abstraction
just above that, and so on.  If the number exceeds the number of enclosing lambda abstractions,
then it is a free variable.

But this, too, has some drawbacks, so people have devised a number of other approaches:

*   "maps" ([Viewing Terms through Maps](https://www.mathematik.uni-muenchen.de/~schwicht/papers/lambda13/lamtheory8.pdf) (PDF), Sato et al., 1980)
    (see also [these slides](https://www.fos.kuis.kyoto-u.ac.jp/~masahiko/papers/mask.pdf) (PDF) from 2012)
*   "nominal techniques" ([A New Approach to Syntax](http://www.gabbay.org.uk/papers/newaas.pdf) (PDF), Gabbay and Pitts, 1999)
*   "locally nameless" ([I am not a number](http://www.e-pig.org/downloads/notanum.pdf) (PDF), McBride and McKinna, 2004)
*   "bound" ([bound: Making de Bruijn Succ Less](https://www.schoolofhaskell.com/user/edwardk/bound), Kmett, 2013)

among others.

But the point I would like to make in this article is this:  At some level of abstraction
_it does not matter_ which approach is chosen _as long as_ the approach satisfies the
essential properties that we require of lambda terms.

To this end, this article presents an abstract data type (ADT) for lambda terms, which we
call **Lariat**, consisting of four operations.  The actual, concrete data structure
in which they are stored, and the actual, concrete mechanism by which names
become bound to terms, are of no consequence (and may well be hidden
from the programmer) so long as the implementation of the operations conforms
to the stated specification.

This ADT is designed for simplicity and elegance rather than performance.  It is a minimal
formulation that does not necessarily make any of commonly-used manipulations
of lambda terms efficient.

This ADT has two properties, intended to contribute to its elegance.
Firstly, it can represent only _proper_ lambda terms; that is, it is not possible
for a lambda term constructed by the Lariat operations to contain an invalid
bound variable.

Secondly, it is _total_ in the sense that all operations are defined
for all inputs that conform to their type signatures.  There are no conditions
(such as trying to pop from an empty stack in a stack ADT) where the result is
undefined, nor any defined to return an error condition.  This totality does, however,
come at the cost of the operations being higher-order and with polymorphic types.

For more background information, see the [Discussion](#discussion) section below.

Names
-----

Lambda terms are essentially about name binding, and in any explication of name binding,
we must deal with names.  As of 0.3, Lariat requires only two properties of names.

Firstly, it must be possible to compare two names for equality.  This is
required for operations that replace free variables that have a given name
with a value -- there must be some way for them to check that the free
variable has the name that they are seeking.

Secondly, given a set of names, it must be possible to generate a new name that
is not equal to any of the names in the set (a so-called "fresh" name).  This is
required to properly implement the `destruct` operation.  If names are modelled
as character strings, obtaining a fresh name could be as simple as finding the
longest string of a set of strings, and prepending `"a"` to it.

Note that, although neither of these properties is exposed as an operation,
it would be reasonable for a practical implementation of Lariat to expose
them so.  It would also be reasonable to provide
other operations on names, such as constructing a new name from a textual
representation, rendering a given name to a canonical textual representation,
and so forth.  From the perspective of Lariat itself these are ancillary
operations, and as such will not be defined in this document.

Terms
-----

We now list the four operations available for manipulating terms.

### `var(n: name): term`

Given a name _n_, return a _free variable_ with the name _n_.

> **Note**: A free variable is a term; it can be passed to any operation
> that expects a term.

### `app(t: term, u: term): term`

Given a term _t_ and a term _u_, return an _application term_
which contains _t_ as its first subterm and _u_ as its second
subterm.

> **Note**: An application term is a term that behaves just like an
> ordered pair of terms.

### `abs(n: name, t: term): term`

Given a name _n_ and a term _t_, return an _abstraction term_
containing _t_', where _t_' is a version of _t_ where all free
variables named _n_ inside _t_' have been replaced with
bound variables.  These bound variables are bound to
the returned abstraction term.

> **Note**: we may consider a bound variable to be a term, but
> the user of the abstract data type cannot work with bound variables
> directly, so it is unlike all other kinds of terms in that respect.
> A bound variable is always bound to a particular abstraction term.
> In the case of `abs`, the abstraction term to which variables are
> bound is always the term returned by the `abs` operation.

> **Note**: an abstraction term contains one subterm.  This
> subterm cannot be extracted directly, as it may contain bound
> variables, which the user cannot work with directly.

### `destruct(t: term, f1: fun, f2: fun, f3: fun): X`

Given a term _t_ and three functions _f1_, _f2_, and _f3_
(each with a different signature, described below),
choose one of the three functions based on the structure of
_t_, and evaluate it, returning what it returns.

If _t_ is a free variable, evaluate _f1_(_n_) where _n_ is the name
of the free variable _t_.

If _t_ is an application term, evaluate _f2_(_u_, _v_) where _u_ is
the first subterm of _t_ and _v_ is the second subterm of _t_.

If _t_ is an abstraction term, evaluate _f3_(_u_, _n_) where _u_ is
a version of _t_ where all bound variables in _u_ that were bound to
_u_ itself have been replaced by _n_, where _n_ is a fresh name
(i.e. a name that does not occur in any free variable in any subterm
of _u_).

> **Note**: as stated above, a bound variable is always bound
> to an abstraction term.  The bound variables that are replaced by the
> `destruct` of an abstraction term are always and only those that are
> bound to the abstraction term being `destruct`ed.

> **Note**: the `destruct` operation's signature shown above was abbreviated to make
> it less intimidating.  The full signature would be
> 
>     destruct(t: term, f1: fun(n: name): X, f2: fun(u: term, v: term): X, f3: fun(u: term, n: name): X): X
> 

> **Note**: see the section on "Names" above for the basic
> requirements for obtaining a fresh name.

Some Examples
-------------

We will now give some concrete examples of how these operations
can be used.  But first, we would like to emphasize that
Lariat is an ADT for lambda _terms_, not the lambda
_calculus_.  Naturally, one ought to be able to write a lambda calculus
normalizer using these operations (and this will be one of our goals in
the next section), but one is not restricted to that activity.  The terms
constructed using the Lariat operations may be used for any purpose
for which terms-with-name-binding might be useful.

### Example 1

A common task is to obtain the set of free variables present
in a lambda term.  This is not difficult; we only need to
remember that every time we `destruct` an abstraction term, we
introduce a fresh free variable of our own, to keep track of
these, and make sure not to include any of them when we
report the free variables we found.

> **Note**: In the following pseudocode, `+` is the set union operator.

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
and again to ensure the application term's first subterm
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

In fact, we _could_ merge this implementation with the
implementation of `resolve` and this would save a call
to `destruct`; but this would be merely an optimisation.
It is left as an exercise to any reader who may be so
motivated to undertake it.

### Example 4

The next task would be to search through a lambda term,
looking for a candidate application term to reduce, and
reducing it.  The pseudocode below returns a pair
`[bool, term]` where the boolean value indicates whether
the term has been rewritten by the call or not.  It
implements a leftmost-outermost reduction strategy.

    let reduce = fun(t) ->
        if is_beta_reducible(t) then
            [true, beta(t)]
        else
            destruct(t,
                fun(n) -> [false, var(n)],
                fun(u, v) ->
                    let
                        [has_rewritten, new_u] = reduce(u)
                    in
                        if has_rewritten then
                            [true, app(new_u, v)]
                        else
                            let
                                [has_rewritten, new_v] = reduce(v)
                            in
                                if has_rewritten then
                                    [true, app(u, new_v)]
                                else
                                    [false, app(new_u, new_v)],
                fun(t) -> [false, t]
            )

From there it's just a hop, a skip, and a jump
to a proper lambda term normalizer:

    let normalize(t) ->
        let
            [has_rewritten, new_t] = reduce(t)
        in
            if has_rewritten then
                normalize(new_t)
            else
                t

Discussion
----------

### Prior work: Paulson's exercise

The idea of formulating an ADT for lambda terms is not a new one.
In Chapter 9 of "ML for the Working Programmer", 1st ed. (1991),
Lawrence Paulson develops an implementation of lambda terms in ML and notes
that:

> Signature LAMBDA_NAMELESS is concrete, revealing all the internal
> details.  [...]  An abstract signature for the λ-calculus would
> provide operations upon λ-terms themselves, hiding their
> representation.

So the idea is an established one; but if so, why does one see so few instances
of it out in the wild?  I think it's this: most lambda term manipulation code
sees actual use only academic contexts, most usually in such things as theorem provers.
These are contexts that don't greatly benefit from the software
engineering principle of being able to swap out one implementation
of an interface with an alternative implementation.  Indeed, in a
theorem proving context, an extra level of abstraction may just
be another burden that the mechanical reasoning methods need to
deal with, with no other benefit.  So concrete data types are used,
because concrete data types are sufficient.

In the context of Lariat, however, the ADT is the object of study in its own right.

Now, here's the part I elided from the above quoted paragraph:

> Many values of type _term_ are **improper**: they do not correspond to
> real λ-terms because they contain unmatched bound variable indices.
> [...]  _abstract_ returns improper terms and _subst_ expects them.

And at the end of the section, he poses Exercise 9.16:

> Define a signature for the λ-calculus that hides its internal representation.
> It should specify predicates to test whether a λ-term is a variable,
> an abstraction, or an application, and specify functions for abstraction
> and substitution.

However, as he mentioned earlier, these operations produce and expect improper
terms; so he appears to be asking for an abstract representation of lambda terms
that includes improper lambda terms.  [[Footnote 1]](#footnote-1)
I would argue that such an ADT has a lot less value as an abstraction
than an ADT in which only proper lambda terms can be represented.
[[Footnote 2]](#footnote-2)

Although it was not in direct response to this exercise (which I hadn't
seen for years until I came across it again), it was consideration
of this point -- how does one formulate an ADT that represents only
proper lambda terms? -- that led me to formulate Lariat.

### The role of `destruct`

`var`, `app`, and `abs` construct terms, while `destruct` takes them apart.
Constructing terms is the easy part; it's taking them apart properly that's
hard.

`destruct` is a "destructorizer" in the sense described in 
[this article on Destructorizers](http://github.com/cpressey/Destructorizers).
In fact, this use case of "taking apart" lambda terms was one
of the major motivations for formulating the destructorizer
concept.

Although it was not specifically intended, `destruct` is also what permits
the ADT to be "total" in the sense that there are no operations that are
undefined.

### Equality modulo renaming of bound variables

When working with lambda terms, one is often concerned with
comparing two lambda terms for equality, modulo renaming of bound
variables.  We haven't introduced such an operation because it should
be possible to build such an operation using `destruct`; basically,
render the two terms as text (or some other concrete representation),
then compare the texts for equality.  (This does however require that
thete is an operation for rendering a name to its textual representation,
and also that the procedure for obtaining a fresh name is deterministic,
so that the fresh names generated when `destruct`ing two equal abstractions,
match up in both of the terms.)

Of course, such an operation could be provided as a native
operation for performance or convenience.  (This is one of the nice
things about ADTs -- they can be sub-ADTs of a larger ADT.)  Similarly,
although we have shown that we can implement `freevars` using the operations
of the ADT, the definition of the `destruct` operation essentially requires
that something equivalent to it already exists, and it could be exposed to
the user as well.

### The possibility of an algebraic formulation

The ADT that has been described in this document has been described
quite precisely (I hope) but not formally.  A direction that this
work could be taken in would be to produce a definition of Lariat that
is actually formal, i.e. in the form of an equational theory, or
equivalently, an algebra.

There are reasons to believe this is not impossible.  In
[The Lambda Calculus is Algebraic](https://www.mscs.dal.ca/~selinger/papers/combinatory.pdf) (PDF)
(Selinger, 1996) an algebra equivalent to the lambda calculus is
formed by treating free variables as "indeterminates" (although
I must admit I'm not entirely certain what is meant by that).
Additionally, section 1.3 of [Language Prototyping: An Algebraic Specification Approach](https://archive.org/details/languageprototyp0000unse)
(1996; van Deursen, Heering, Klint eds., borrowable online at archive.org)
gives a definition of the lambda calculus in the algebraic definition
language ASF+SDF, which comes fairly close to conventional equational logic
(although it does contain extras such as conditional equations).

However, in Lariat, `destruct` is a "higher-order" operation,
in the sense that it takes functions as parameters, and this may
well complicate the task of defining an equational theory based
on Lariat, or it may complicate the resulting equational theory.
We'll talk about that in the next section.

### Variation: Partial Lariat

To support the effort of formulating an algebra based on Lariat,
or for any other purposes which it may suite, it's worth looking at the possibility
of replacing `destruct` with a set of "first-order" operations.

When I first started working out Lariat, I thought that using a
destructorizer would be essential to the problem of being able to
destruct an abstraction term and have the result be a proper lambda term.
It's not as essential as I thought.  What `destruct` does when given
an abstraction term is, basically, to form a free variable with a
fresh name (one that does not occur in the abstraction term) and
`resolve` (as defined in the examples above) the abstraction term with it.
If the ADT were to have discrete operations for picking a fresh name
given a lambda term, and for `resolve`, these could be applied
"manually", and in this manner the user could destruct abstraction
terms just the same as `destruct` does.

There are subtle differences: `resolve` would need to be an intrinsic operation
which is exposed by the ADT, rather that derived from the basic operations
of Lariat.  It also gives the user the freedom to apply `resolve` with whatever
they wish, while in Lariat, `destruct` can only apply this action with a fresh
variable that it itself has chosen, which is significantly more restrictive.

A version of Lariat without `destruct` would also need operations
for testing if a term is an abstraction term, vs. an application
term, vs. a free variable.  The operation of extracting the first
and second values from an application term (basically, the theory
of ordered pairs) would not be sensibly defined for abstraction
terms or free variables, and so this version of the ADT would be
partial rather than total, thus the name "Partial Lariat".

Footnotes
---------

#### Footnote 1

Either that or, based on his remark about
an "abstract signature for the λ-calculus", he intended the operations in this
exercise to be on the level of the lambda calculus, i.e. beta-reduction and
normalization?  But that's not what he wrote, and lacking a copy of the 2nd
edition to see if this has been corrected, I shall take him at his word.

#### Footnote 2

For more information on this philosophy, see "Parse, don't Validate";
in particular, [LCF-style-ND](http://github.com/cpressey/LCF-style-ND)
illustrates how it applies to theorem objects in an LCF-style theorem prover;
and it applies here too.
