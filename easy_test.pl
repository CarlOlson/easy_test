
:- module(easy_test, [expect/1]).

:- op(510, fx,  user:(expect)).
:- op(501, xfx, user:(to)).
:- op(500, fy,  user:(eq)).
:- op(500, fy,  user:(not_eq)).

expect(A) :- A.

to(A, B) :-
    call(B, A, Result),
    check_result(A, B, Result), !.

eq(B, A, Result) :-
    check_existance(A, 1),
    (call(A, Av) *->
	 (Av == B *->
	      Result = t;
	  Result = result(atMb, Av, B, " should be "));
     Result = result(atMb, Av, B, " failed, should be ")).

not_eq(B, A, Result) :-
    check_existance(A, 1),
    (call(A, Av) *->
	 (Av \== B *->
	      Result = t;
	  Result = result(atM, Av, B, " should be false"));
     Result = result(atMb, Av, B, " failed, should not be ")).

fail(A, Result) :-
    check_existance(A, 0),
    (call(A) *->
	 Result = result(aM, A, _, " should fail");
     Result = t).


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
    print_result(Type,
		 [ATerm, A, Av],
		 [BTerm, Bv],
		 Msg),
    fail.

print_result(atM, [ATerm|_], _, Msg) :-
    write_term(ATerm, [spacing(next_argument)]),
    write(Msg),
    nl.
print_result(atMb, [ATerm|_], [BTerm|_], Msg) :-
    write_term(ATerm, [spacing(next_argument)]),
    write(Msg),
    write_term(BTerm, [spacing(next_argument)]),
    nl.
print_result(aM, [_,A,_], _, Msg) :-
    write_term(A, [spacing(next_argument)]),
    write(Msg),
    nl.

fail_msg :- write("!!! FAIL\n\t").
