
:- module(easy_test, [expect/1]).

:- op(510, fx,  user:(expect)).
:- op(501, xfx, user:(to)).
:- op(500, fy,  user:(eq)).
:- op(500, fy,  user:(not_eq)).


expect(A) :- A.

to(A, B) :-
    check_existance(A, 1),
    call(A, Av),
    call(B, Av, Result),
    check_result(A, B, Result), !.

eq(B, A, t) :-
    A == B, !.
eq(B, A, result(aMb, A, B, " should be ")).

not_eq(B, A, t) :-
    A \== B, !.
not_eq(B, A, result(aM, A, B, " should be false")).


check_existance(Term, ExtraCount) :-
    Term =.. [F | Args],
    length(Args, SubCount),
    ArgCount is SubCount + ExtraCount,
    functor(Pred, F, ArgCount),

    (current_predicate(_, Pred)
     *-> true ;

     fail_msg,
     write(F), write("/"), write(ArgCount),
     write(" is not defined"), nl,
     fail).


check_result(_, _, t).
check_result(A, _, result(Type, Av, Bv, Msg)) :-
    A =.. [Fa | Arga],

    append(Arga, [Av], ATermArgs),
    ATerm =.. [Fa | ATermArgs],

    append(Arga, [Bv], BTermArgs),
    BTerm =.. [Fa | BTermArgs],

    fail_msg,
    print_result(Type, ATerm, BTerm, Msg),

    fail.

print_result(aM, A, _, Msg) :-
    write_term(A, [spacing(next_argument)]),
    write(Msg),
    nl.
print_result(aMb, A, B, Msg) :-
    write_term(A, [spacing(next_argument)]),
    write(Msg),
    write_term(B, [spacing(next_argument)]),
    nl.

fail_msg :- write("!!! FAIL\n\t").
