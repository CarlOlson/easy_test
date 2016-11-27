
:- ['easy_test'].

:- initialization main.

:- dynamic sub1/2.

add1(N, N).

test :-
    \+ describe(add1/2,
		[
		    expect it(0) to eq 1
		]),

    \+ describe(something/2, []),

    \+ describe("strings should be ok",
		[
		    expect add1(0, 1)
		]),

    \+ describe("lists only...", not_list),

    write('DONE').

main :-
    test, halt;
    halt.

%%%% Expected output:
%% !!! FAIL
%% 	add1(0, 0) should be add1(0, 1)
%% !!! FAIL
%% 	something/2 is not defined
%% !!! FAIL
%% 	add1(0, 1) should succeed
%% !!! FAIL
%% 	in describe("lists only...",'Tests'), tests should be a list
%% DONE
