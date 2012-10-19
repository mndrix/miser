%:- module(sort, []).

:- use_module(library(random)).

% each clause is an observation of runtime cost for a miserly predicate
:- dynamic observation/3.

% implementations(Predicate, Implementations)
%
% True if Predicate has a list of Implementations
:- dynamic implementations/2.


% miserly(PredicateIndicator, Implementations)
%
% Creates a predicate (name and arity determined by
% PredicateIndicator) which self-optimizes by choosing the fastest
% implementation from among Implementations.
miserly(Predicate, Implementations) :-
    (dynamic Predicate),
    assertz(implementations(Predicate, Implementations)),
    Functor/Arity = Predicate,
    length(Args, Arity),
    Head =.. [Functor|Args],
    Body = ( measure_one(Predicate, Args),
             trim_implementations(Predicate)
           ),
    assertz(Head :- Body).


:- miserly(miser_sort/2, [tiny_sort,permutation_sort,merge_sort,quick_sort]).

% measure a single, working implementation and record results
% in the database. removes failing implementations if any are encountered
measure_one(Predicate, Arguments) :-
    random_implementation(Predicate, Chosen), % infinite choice points
        format('chose ~p~n', [Chosen]),
        (   measure_cost(Chosen, Arguments, Cost)
        ->  true
        ;   remove_implementation(Predicate, Chosen, _),
            fail
        ),
    !,
    observe(Predicate, Chosen, Cost),
    format('  cost ~D~n', [Cost]).

% randomly choose an implementation for Predicate, providing
% infinite choice points choosing randomly again on each backtrack
random_implementation(Predicate, Chosen) :-
    repeat,
    implementations(Predicate, Choices),
    ( Choices=[] -> throw('No implementations to choose from') ; true),
    random_member(Chosen, Choices).

% record results of measuring an implementation's performance
observe(Predicate, Implementation, Cost) :-
    assertz(observation(Predicate, Implementation, Cost)).

% remove cost measurement results (opposite of observe/3)
forget(Predicate, Implementation) :-
    retractall(observation(Predicate, Implementation,_)).

% macro creates this to count observations that have happened so far
observation_count(Predicate, Count) :-
    findall(Name, observation(Predicate, Name, _), Names),
    length(Names, Count).

% macro creates this to discard losing implementations
trim_implementations(Predicate) :-
    observation_count(Predicate, ObservationCount),
    ObservationCount > 5,
    !,

    % find the most costly implementation we've seen
    most_costly_implementation(Predicate, MostCostly),

    % ... remove it from available choices
    remove_implementation(Predicate, MostCostly, Keepers),

    (   Keepers=[]       ->  found_winner(Predicate, MostCostly)
    ;   Keepers=[Winner] ->  found_winner(Predicate, Winner)
    ;   format('discarding ~p~n', [MostCostly]),
        forget(Predicate, MostCostly)
    ).
trim_implementations(_).

% true if MostCostly is the implementation with highest measured
% cost in our observations so far
most_costly_implementation(Predicate, MostCostly) :-
    findall(Cost-Name, implementation_cost(Predicate, Name, Cost), Costs),
    keysort(Costs, AscendingCost),
    reverse(AscendingCost, [_-MostCostly|_]).

% remove a single implementation from the list of choices
remove_implementation(Predicate, Needle, Leftover) :-
    implementations(Predicate, Choices),
    once(select(Needle, Choices, Leftover)),
    retractall(implementations(Predicate,_)),
    assertz(implementations(Predicate, Leftover)).


% true if Predicate has an implementation Name whose average observed
% cost is AvgCost
implementation_cost(Pred, Name, AvgCost) :-
    aggregate(count, Pred^Cost^observation(Pred, Name, Cost), Count),
    aggregate(sum(Cost), Pred^observation(Pred, Name, Cost), TotalCost),
    AvgCost is TotalCost / Count.

% macro creates this for making the winner permanent
found_winner(Predicate, Winner) :-
    format('found a winner: ~p~n', [Winner]),

    % prepare to erase the old definition
    Functor/Arity = Predicate,
    functor(Term, Functor, Arity),
    clause(Term, _, OldClause),  % should be exactly one OldClause

    % make Functor an alias for Winner (same arity)
    length(Args, Arity),
    Head =.. [Functor|Args],
    Body =.. [Winner|Args],
    assertz(Head :- Body),

    % erase the old definition and clean up
    erase(OldClause),
    compile_predicates([Predicate]),
    retractall(implementations(Predicate, _)),
    retractall(observation(Predicate,_,_)).


% measures the cost of calling a goal (by some reasonable metric)
measure_cost(Implementation, Arguments, Cost) :-
    Goal =.. [Implementation|Arguments],
    statistics(inferences, Before),
    call(Goal),
    statistics(inferences, After),
    Cost is After - Before.



% ------ several sort implementations are below here -------

verify :-
    implementations(miser_sort/2, Sorters),
    forall(member(Sorter, Sorters), verify(Sorter)).

verify(Sorter) :-
    format('Verifying ~p ... ', [Sorter]),
    flush_output,
    forall(between(1,100, _), verify_work(Sorter)),
    format('done~n').

verify_work(Sorter) :-
    (Sorter=tiny_sort -> MaxLen=3
    ;Sorter=permutation_sort -> MaxLen=10
    ;MaxLen=30
    ),
    random_list(MaxLen, Xs),
    (   call(Sorter, Xs, Sorted)
    ->  ( is_sorted(Sorted)
        -> true
        ;   format('BAD! ~w => ~w~n', [Xs, Sorted])
        )
    ;   format('QUIT ~w~n', [Xs])
    ).

random_list(MaxLen, Xs) :-
    Len is random(MaxLen),
    length(Xs, Len),
    lucky(Xs).


lucky([]).
lucky([X|Xs]) :-
    X is random(100),
    lucky(Xs).

% true if elements of Xs are sorted in increasing order
is_sorted([]).
is_sorted([_]) :- !.
is_sorted([X,Y|Rest]) :-
    X @=< Y,
    is_sorted([Y|Rest]).


% test every permutation of Xs looking for one that's sorted
permutation_sort(Xs, Sorted) :-
    length(Xs, Len),
    Len < 10,   % takes too long on longer lists
    permutation(Xs, Sorted),
    is_sorted(Sorted),
    !.  % only want the first sorted permutation


% hardcoded sort for tiny lists
tiny_sort([],[]).
tiny_sort([X], [X]) :- !.
tiny_sort([A,B], [X,Y]) :-
    (   A @=< B
    ->  X=A, Y=B
    ;   X=B, Y=A
    ).
tiny_sort([A,B,C], Sorted) :-
    (A @=< B -> AB=ab ; AB=ba),
    (A @=< C -> AC=ac ; AC=ca),
    (B @=< C -> BC=bc ; BC=cb),
    tiny_sort_table(AB, AC, BC, A, B, C, Sorted).

tiny_sort_table(ab, ac, bc, A, B, C, [A,B,C]) :- !.
tiny_sort_table(ab, ac, cb, A, B, C, [A,C,B]) :- !.
tiny_sort_table(ab, ca,  _, A, B, C, [C,A,B]) :- !.
tiny_sort_table(ba, ac,  _, A, B, C, [B,A,C]) :- !.
tiny_sort_table(ba, ca, bc, A, B, C, [B,C,A]) :- !.
tiny_sort_table(ba, ca, cb, A, B, C, [C,B,A]) :- !.


% traditional merge sort
merge_sort([], []) :- !.
merge_sort([X], [X]) :- !.
merge_sort(Xs, Ys) :-
    divide(Xs, Left0, Right0),
    merge_sort(Left0, Left),
    merge_sort(Right0, Right),
    merge(Left, Right, Ys).

divide(Xs, Left, Right) :-
    length(Xs, Len),
    Target is round(Len/2),
    divide_loop(Target, Xs, [], Left, Right).

divide_loop(0, Right, Accum, Left, Right) :-
    reverse(Accum, Left),
    !.
divide_loop(N, [X|Xs], Accum, Left, Right) :-
    N1 is N-1,
    divide_loop(N1, Xs, [X|Accum], Left, Right).

merge([], Ys, Ys) :- !.
merge(Xs, [], Xs) :- !.
merge([X|Xs], [Y|Ys], [Smaller|Rest]) :-
    (   X @=< Y
    ->  Smaller = X,
        merge(Xs, [Y|Ys], Rest)
    ;   Smaller = Y,
        merge([X|Xs], Ys, Rest)
    ).


% standard quick sort
quick_sort([],[]).
quick_sort([X], [X]) :- !.
quick_sort([Pivot|Xs], Sorted) :-
    partition('@>'(Pivot), Xs, Left, Right),
    quick_sort(Left, LeftSorted),
    quick_sort(Right, RightSorted),
    append(LeftSorted, [Pivot|RightSorted], Sorted).

