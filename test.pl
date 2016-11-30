
:- module(tests, [add1/2]).
:- use_module(easy_test).

:- initialization main.

%% add1(?, ?)
% a faulty implementation of add1/2
add1(-1, _) :- !, fail.
add1(N, N).

acceptance :-
    expect describe(add1/2, [ expect it( 0) to eq 1 ])
    output_to match "should be add1(0,1)",
    expect describe(add1/2, [ expect it(-1) to eq 0 ])
    output_to match "failed, should be add1(-1,0)",

    expect describe(add1/2, [ expect it( 0) to not_eq  0 ])
    output_to match "should be false",
    expect describe(add1/2, [ expect it(-1) to not_eq -1 ])
    output_to match "failed, should not be add1(-1,-1)",

    expect describe(add1/2, [ expect it(-2, _) to fail ])
    output_to match "should fail",
    expect describe(add1/2, [ expect it(-1, _) to succeed ])
    output_to match "should succeed",

    expect describe(something/2, [])
    output_to match "something/2 is not defined",

    expect describe("strings should be ok", [ expect add1(0, 1) ])
    output_to match "should succeed",

    expect describe("lists only...", not_list)
    output_to match "should be a list".

unit :-
    describe(describe/2,
    	     [
		 %% NOTE this is a feature :-)
    		 %% "it/N enforces arity specified in describe/2",
    		 %% expect it(add1/2, [ expect it(x, y) to eq z ])
		 %% output_to match "arity",

		 "only accepts valid method predicates",
		 expect it(add1/2, []) to succeed,
		 expect it("add1"/2, []) to fail
    	     ]).

test :-
    unit,
    acceptance.

main :-
    (test ->
	 write('SUCCESS');
     write('FAIL')),
    halt.
