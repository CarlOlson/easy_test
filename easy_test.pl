
:- module(easy_test, [expect/1, describe/2]).
:- use_module(library(clpfd)).

:- op(510, fx,  user:(expect)).
:- op(501, xfx, user:(to)).
:- op(500, fy,  user:(always)).
:- op(500, fy,  user:(match)).
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
    push_arg(A, Av, Call),
    push_arg(A, B, Expected),
    (call(Call) *->
	 (Av == B *->
	      Result = t;
	  Result = result("~p should be ~p~n", [Call, Expected])
	 );
     Result = result("~p failed, should be ~p~n", [Call, Expected])).

not_eq(B, A, Result) :-
    check_existance(A, 1),
    push_arg(A, Av, Call),
    push_arg(A, B, Expected),
    (call(Call) *->
	 (Av \== B *->
	      Result = t;
	  Result = result("~p should be false~n", [Call]));
     Result = result("~p failed, should not be ~p~n", [Call, Expected])).

always(B, A, Result) :-
    bind_fd(A, Ss),
    maplist(indomain, Ss),

    (call(B, A, Result),
     Result \= t, !);
    Result = t.

%% TODO truncate result output
match(B, A, Result) :-
    callable(A),
    string(B),
    push_arg(A, Av, Call),
    (call(Call) ->
	 (sub_string(Av, _, _, _, B) ->
	      Result = t;
	  Result = result("In ~p, ~p should match ~p~n", [Call, Av, B]));
     Result = result("~p failed, ~p should match ~p~n", [Call, Av, B])).
match(B, A, Result) :-
    string(A),
    string(B),
    (sub_string(A, _, _, _, B) *->
	 Result = t;
     Result = result("~p should match ~p~n", [A, B])).
match(B, A, result("~p and ~p should be strings for match", [A, B])).

fail(A, Result) :-
    check_existance(A, 0),
    (call(A) *->
	 Result = result("~p should fail~n", [A]);
     Result = t).

succeed(A, Result) :-
    check_existance(A, 0),
    (call(A) *->
	 Result = t;
     Result = result("~p should succeed~n", [A])).


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
describe(_, Tests) :-
    is_list(Tests), !,
    remove_comments(Tests, Tests1),

    check_callable(Tests1),
    list_to_callable(Tests1, Tests0),
    call(Tests0).
describe(Pred, _) :-
    log(fail,
	"in ~p, Tests should be a list~n",
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

     log(fail, "~p/~p is not defined~n", [F, ArgCount]),
     fail).

check_result(_, _, t).
check_result(_, _, result(Msg, Args)) :-
    log(fail, Msg, Args),
    fail.

push_arg(Term, Arg, Out) :-
    Term =.. [F | Args],
    append(Args, [Arg], NewArgs),
    Out =.. [F | NewArgs].

log(fail, Msg, Args) :-
    format("!!! FAIL\n\t"),
    format(Msg, Args).
