
:- module(easy_test, [expect/1, describe/2]).
:- use_module(library(clpfd)).

:- op(510, fx,  user:(expect)).
:- op(501, xfx, user:(to)).
:- op(500, fy,  user:(always)).
:- op(500, fy,  user:(eq)).
:- op(500, fy,  user:(not_eq)).

expect(to(A, B)) :-
    to(A, B), !.
expect(A) :-
    \+ A =.. [to|_],
    to(A, succeed).

to(A, B) :-
    call(B, A, Result), !,
    check_result(A, B, Result), !.

eq(B, A, Result) :-
    check_existance(A, 1),
    (call(A, Av) *->
	 (Av == B *->
	      Result = t;
	  Result = result(atMb, Av, B, " should be "));
     Result = result(atMb, '_', B, " failed, should be ")).

not_eq(B, A, Result) :-
    check_existance(A, 1),
    (call(A, Av) *->
	 (Av \== B *->
	      Result = t;
	  Result = result(atM, Av, B, " should be false"));
     Result = result(atMb, '_', B, " failed, should not be ")).

always(B, A, Result) :-
    bind_fd(A, Ss),
    maplist(indomain, Ss),

    (call(B, A, Result),
     Result \= t, !);
    Result = t.

fail(A, Result) :-
    check_existance(A, 0),
    (call(A) *->
	 Result = result(aM, A, _, " should fail");
     Result = t).

succeed(A, Result) :-
    check_existance(A, 0),
    (call(A) *->
	 Result = t;
     Result = result(aM, A, _, " should succeed")).


describe(Pred, Tests) :-
    compound(Pred),
    is_list(Tests),
    current_predicate(Pred), !,

    Pred =.. ['/', F, _],
    transform_it(F, Tests, Tests0),

    term_string(Pred, PredString),
    describe(PredString, Tests0).
describe(Pred, _) :-
    compound(Pred), !,
    log(fail, "~p is not defined~n", Pred),
    fail.
describe(Pred, Tests) :-
    is_list(Tests), !,
    remove_comments(Tests, Tests1),

    check_callable(Tests1),
    list_to_callable(Tests1, Tests0),
    call(Tests0).
describe(Pred, _) :-
    log(fail,
	"in ~p, tests should be a list~n",
	[describe(Pred, 'Tests')]),
    fail.

bind_fd(Term, [Term]) :-
    clpfd:fd_var(Term), !.
bind_fd(Term, []) :-
    var(Term), !.
bind_fd([T|Ts], Out) :-
    !,
    bind_fd(T, O1),
    bind_fd(Ts, O2),
    append(O1, O2, Out).
bind_fd(Term, Out) :-
    compound(Term), !,
    Term =.. [_|Args],
    bind_fd(Args, Out).
bind_fd(_, []).

remove_comments(L, L0) :-
    exclude(string, L, L0).

check_callable(L) :-
    include(callable, L, L).

list_to_callable([], true).
list_to_callable([F|L], Callable) :-
    list_to_callable(L, SubCalls),
    Callable =.. [',', F, SubCalls].

transform_it(_, Term, Term) :-
    var(Term), !.
transform_it(F, Term, [O|Os]) :-
    is_list(Term),
    \+ length(Term, 0), !,
    Term = [T|Ts],
    transform_it(F, T, O),
    transform_it(F, Ts, Os).
transform_it(F, Term, Out) :-
    compound(Term),
    Term =.. [it|Args], !,
    Out  =.. [ F|Args].
transform_it(F, it, F) :- !.
transform_it(F, Term, Out) :-
    compound(Term),
    Term =.. [F2|Args],
    transform_it(F, Args, Args2),
    Out =.. [F2|Args2], !.
transform_it(_, Term, Term) :- !.

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

log(fail, Msg, Args) :-
    fail_msg,
    format(Msg, Args).
