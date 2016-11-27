
:- ['easy_test'].

:- initialization main.

:- dynamic sub1/2.

add1(-1, _) :- !, fail.
add1(N, N).

test :-
    \+ describe(add1/2, [ expect it(0) to eq 1 ]),
    \+ describe(add1/2, [ expect it(-1) to eq 0 ]),

    \+ describe(add1/2, [ expect it(0) to not_eq 0 ]),
    \+ describe(add1/2, [ expect it(-1) to not_eq -1 ]),

    \+ describe(add1/2, [ expect it(-2, _) to fail ]),
    \+ describe(add1/2, [ expect it(-1, _) to succeed ]),

    \+ describe(something/2, []),

    \+ describe("strings should be ok", [ expect add1(0, 1) ]),

    \+ describe("lists only...", not_list),

    write('DONE').

main :-
    test, halt;
    halt.

%%%% Expected output:
%% !!! FAIL
%% 	add1(0,0) should be add1(0,1)
%% !!! FAIL
%% 	add1(-1,_652) failed, should be add1(-1,0)
%% !!! FAIL
%% 	add1(0,0) should be false
%% !!! FAIL
%% 	add1(-1,_652) failed, should not be add1(-1,-1)
%% !!! FAIL
%% 	add1(-2,-2) should fail
%% !!! FAIL
%% 	add1(-1,_394) should succeed
%% !!! FAIL
%% 	something/2 is not defined
%% !!! FAIL
%% 	add1(0,1) should succeed
%% !!! FAIL
%% 	in describe("lists only...",'Tests'), Tests should be a list
%% DONE
