:- use_module(miser_sort).

short(Sorted) :-
    miser_sort([3,7,2], Sorted).
long(Sorted) :-
    miser_sort([45,93,35,23,20,52,12,77,56,25,12], Sorted).
