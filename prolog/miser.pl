:- module(miser, [miserly/2]).

% TODO script that calls miser_sort/2 from multiple predicates

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
:- meta_predicate miserly(0,+).
miserly(Predicate, Implementations) :-
    Module:Name/Arity = Predicate,
    functor(Term0, Name, Arity),
    format(atom(Base), 'miserly_~w_', [Name/Arity]),
    assertz((
        user:goal_expansion(Term0, Module:Term) :-
            gensym(Base, Sym),
            rename_term(Term0, Sym, Term),
            generate_optimizing_predicate(Module:Sym/Arity, Implementations)
    )).

rename_term(Term0, NewName, Term) :-
    Term0 =.. [_|Args],
    Term  =.. [NewName|Args].

:- meta_predicate generate_optimizing_predicate(0,+).
generate_optimizing_predicate(Predicate, Implementations) :-
    Module:Functor/Arity = Predicate,
    (dynamic Predicate),
    maplist(qualify(Module), Implementations, Qualified),
    assertz(miser:implementations(Predicate, Qualified)),
    length(Args, Arity),
    Head =.. [Functor|Args],
    Body = ( miser:measure_one(Predicate, Args),
             miser:trim_implementations(Predicate)
           ),
    Module:assertz(Head :- Body).

qualify(Module, Predicate, Module:Predicate).


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
    once(implementations(Predicate, Choices)),
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
    Module:Functor/Arity = Predicate,
    functor(Term, Functor, Arity),
    clause(Module:Term, _, OldClause),  % should be exactly one OldClause

    % make Functor an alias for Winner (same arity)
    length(Args, Arity),
    quniv(Head, Functor, Args),
    quniv(Body, Winner, Args),
    Module:assertz(Head :- Body),

    % erase the old definition and clean up
    erase(OldClause),
    compile_predicates([Predicate]),
    retractall(implementations(Predicate, _)),
    retractall(observation(Predicate,_,_)).

% like =../2 (aka "univ") but allows module-qualified functors
quniv(Module:Term, Module:Functor, Args) :-
    Term =.. [Functor|Args],
    !.
quniv(Term, Functor, Args) :-
    Term =.. [Functor|Args].


% measures the cost of calling a goal (by some reasonable metric)
measure_cost(Implementation, Arguments, Cost) :-
    quniv(Goal, Implementation, Arguments),
    statistics(inferences, Before),
    call(Goal),
    statistics(inferences, After),
    Cost is After - Before.
