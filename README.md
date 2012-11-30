Usage
-----

This library is a Prolog macro generator for
**building self-optimizing predicates**.

Let's say you want to sort a list.  Should you use merge sort, quick sort,
counting sort or maybe a hand-coded sort that's optimized for tiny lists?
The best choice depends on CPU cache architectures, compiler optimizations,
runtime data characteristics, etc.  It can be very difficult to know, a priori,
which algorithm is "best".  Even if you choose correctly today, the best
choice is likely to change as your software evolves over time.

Self-optimizing predicates solve this problem by choosing the best available
algorithm at runtime based on live performance metrics.  During a warm up
period, `miser` randomly chooses algorithms while timing their performance.
After enough performance data is collected, it permanently swaps the best
algorithm into place.  It does this separately for each predicate call site.

Here's a sketch of a `best_sort/2` predicate using `miser`:

```prolog
:- module(best_sort, []).
:- use_module(miser).

:- miserly(best_sort/2, [merge_sort,quick_sort,tiny_sort,counting_sort]).

merge_sort(List, Sorted) :-
    % a merge sort implementation goes here

quick_sort(List, Sorted) :-
    % a quick sort implementation goes here


% Implementations can specialize by restricting themselves to certain
% inputs.  Only those implementations that work will be considered.

tiny_sort(List, Sorted) :-
    length(List, Len),
    Len < 4,
    % hand-coded sort for very short lists

counting_sort(List, Sorted) :-
    list_of_integers(List),
    % a counting sort implementation goes here
```

A program can use this new module like this:

```prolog
:- use_module(best_sort).

short_list(Sorted) :-
    best_sort([3,1,2], Sorted).

long_list(Sorted) :-
    best_sort([45,93,35,23,20,52,12,77,56,25,12], Sorted).
```

`short_list/2` and `long_list/2` will use different sort
algorithms, depending on runtime performance characteristics.


Documentation
-------------

`miserly(+NameArity, +Implementations)`
===========

A directive to create a macro with `NameArity` name and arity which chooses
the best implementation from among `Implementations` at run time.


Installation
------------

Using SWI-Prolog 6.3 or later:

    $ swipl
    1 ?- pack_install(miser).



Future Work
-----------

  * Use wall clock time instead of inference count to measure performance
  * Use a statistical model to choose the best runtime implementation
  * Resume search for the best algorithm if a chosen implementation fails after we've already chosen what we thought was the best one
