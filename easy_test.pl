
:- module(easy_test, [
	      expect/1,
	      describe/2,
	      op(710, fx,  expect),
	      op(701, xfx, to),
	      op(701, xfx, output_to),
	      op(700, fy,  always),
	      op(700, fy,  match),
	      op(700, fy,  eq),
	      op(700, fy,  not_eq)]).
:- use_module(library(clpfd)).

expect(to(A, B)) :-
    !, to(A, B).
expect(output_to(A, B)) :-
    !, output_to(A, B).
expect(A) :-
    to(A, succeed).

to(A, B) :-
    call(B, A, Result), !,
    check_result(Result), !.

output_to(A, B) :-
    with_output_to(string(Av), (call(A) -> true; true)),
    call(B, Av, Result), !,
    check_result(Result), !.

eq(B, A, Result) :-
    push_arg(A, Av, Call),
    push_arg(A, B, Expected),
    check_existance(Call),
    (call(Call) *->
	 (Av == B *->
	      Result = t;
	  Result = result("~p should be ~p~n", [Call, Expected])
	 );
     Result = result("~p failed, should be ~p~n", [Call, Expected])).

not_eq(B, A, Result) :-
    push_arg(A, Av, Call),
    push_arg(A, B, Expected),
    check_existance(Call),
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
    check_existance(A),
    (call(A) *->
	 Result = result("~p should fail~n", [A]);
     Result = t).

succeed(A, Result) :-
    check_existance(A),
    (call(A) *->
	 Result = t;
     Result = result("~p should succeed~n", [A])).


describe(F/Arity, Tests) :-
    atom(F),
    integer(Arity),
    is_list(Tests), !,
    (check_existance(F/Arity) ->
	 describe_eval(F, Tests)).
describe(Doc, Tests) :-
    string(Doc),
    is_list(Tests), !,
    describe_eval(Tests).
describe(Pred, _) :-
    log(fail,
	"~p bad args, expected describe(predicate|string, list)~n",
	[describe(Pred, tests)]),
    fail.

describe_eval(F, Tests) :-
    transform_it(F, Tests, Tests1),
    describe_eval(Tests1).
describe_eval(Tests) :-
    remove_comments(Tests, Tests1),
    list_to_callable(Tests1, Tests2),
    call(Tests2).


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
    include(callable, L, L0).

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

check_existance(Term)   :-
    term_to_mfa(Term, MFA),
    current_predicate(MFA).
check_existance(Term) :-
    log(fail, "~p is not defined~n", Term),
    fail.

check_result(t).
check_result(result(Msg, Args)) :-
    log(fail, Msg, Args),
    fail.

push_arg(M:Term, Arg, M:Out) :-
    push_arg(Term, Arg, Out).
push_arg(Term, Arg, Out) :-
    Term =.. [F | Args],
    append(Args, [Arg], NewArgs),
    Out =.. [F | NewArgs].

term_to_mfa(F/A,      F/A).
term_to_mfa(M:Term, M:F/A) :- term_to_mfa(Term, F/A).
term_to_mfa(Term,     F/A) :- functor(Term, F, A).

log(fail, Msg, Args) :-
    format('!!! '),
    ansi_format([fg(red)], 'FAIL\n\t', []),
    format(Msg, Args).
