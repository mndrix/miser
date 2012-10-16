%:- module(sort, []).

:- use_module(library(random)).

% macro creates this to store runtime observations
:- dynamic miser_sort_observation/2.

% macro creates these to list implementation choices
:- dynamic implementation_choices/2.
implementation_choices(miser_sort/2, [tiny_sort,permutation_sort,merge_sort,quick_sort]).


% macro creates this to perform runtime measurements.
% once found, the body is replaced with a caller to the winner
:- dynamic miser_sort/2.
miser_sort(Xs, Sorted) :-
    implementation_choices(miser_sort/2, Choices),
    random_member(Chosen, Choices),
    format('chose ~s~n', [Chosen]),
    Goal =.. [Chosen, Xs, Sorted],
    measure_cost(Goal, Cost),
    format('  cost ~D~n', [Cost]),
    assertz(miser_sort_observation(Chosen, Cost)),
    miser_sort_observation_count(ObservationCount),
    (   ObservationCount > 5
    ->  miser_sort_trim_choices
    ;   true % nothing to do yet
    ).

% macro creates this to count observations that have happened so far
miser_sort_observation_count(Count) :-
    findall(Name, miser_sort_observation(Name, _), Names),
    length(Names, Count).

% macro creates this to discard losing implementations
miser_sort_trim_choices :-
    findall(Cost-Name, miser_sort_aggregate(Name, Cost), Costs),
    keysort(Costs, AscendingCost),
    reverse(AscendingCost, [_-MostCostly|_]),
    implementation_choices(miser_sort/2,Choices),
    once(select(MostCostly, Choices, Keepers)),
    retractall(implementation_choices(miser_sort/2,_)),
    (   Keepers=[]       ->  miser_sort_found_winner(MostCostly)
    ;   Keepers=[Winner] ->  miser_sort_found_winner(Winner)
    ;   format('discarding ~s~n', [MostCostly]),
        assertz(implementation_choices(miser_sort/2, Keepers)),
        retractall(miser_sort_observation(MostCostly,_))
    ).

% macro creates this to aggregate observation results
miser_sort_aggregate(Name, AvgCost) :-
    aggregate(count, Name, Cost^miser_sort_observation(Name, Cost), Count),
    aggregate(sum(Cost), miser_sort_observation(Name, Cost), TotalCost),
    AvgCost is TotalCost / Count.

% macro creates this for making the winner permanent
miser_sort_found_winner(Winner) :-
    format('found a winner: ~s~n', [Winner]),
    clause(miser_sort(_,_), _, OldClause),
    Sort =.. [Winner, Xs, Sorted],
    assertz(miser_sort(Xs, Sorted) :- Sort),
    erase(OldClause),
    compile_predicates([miser_sort/2]),
    retractall(implementation_choices(miser_sort/2, _)),
    retractall(miser_sort_observation(_,_)).


% measures the cost of calling a goal (by some reasonable metric)
measure_cost(Goal, Cost) :-
    statistics(inferences, Before),
    call(Goal),
    statistics(inferences, After),
    Cost is After - Before.



% ------ several sort implementations are below here -------

verify :-
    implementation_choices(miser_sort, Sorters),
    forall(member(Sorter, Sorters), verify(Sorter)).

verify(Sorter) :-
    format('Verifying ~s ... ', [Sorter]),
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

