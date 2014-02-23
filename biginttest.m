:- module biginttest.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module integer.
:- import_module biginteger.
:- import_module int.
:- import_module solutions.
:- import_module list.
:- import_module string.
:- import_module bool.

:- import_module testintegers.

:- pred min1(biginteger::out) is det.
:- pred max1(biginteger::out) is det.

min1(X) :-
    X = biginteger.det_from_string(number1).

max1(X) :-
    X = biginteger.det_from_string(number2).

:- pred min2(biginteger::out) is det.
:- pred max2(biginteger::out) is det.

% 2^(16384*2) / 121
min2(X) :-
    X = biginteger.det_from_string(number3).

max2(X) :-
    X= biginteger.det_from_string(number4).


:- pred min3(biginteger::out) is det.
:- pred max3(biginteger::out) is det.

% (16384^100) / 333
min3(X) :-
    X = biginteger.det_from_string(number5).

max3(X) :-
    X = biginteger.det_from_string(number6).

:- pred min4(biginteger::out) is det.
:- pred max4(biginteger::out) is det.

% (16384^100) * 121 + 37
min4(X) :-
    X = biginteger.det_from_string(number7).

max4(X) :-
    X = biginteger.det_from_string(number8).

:- pred min5(biginteger::out) is det.
:- pred max5(biginteger::out) is det.

% 16384^121 / 13
min5(X) :-
    X = biginteger.det_from_string(number9).

max5(X) :-
    X = biginteger.det_from_string(number10).

:- pred min6(biginteger::out) is det.
:- pred max6(biginteger::out) is det.

% (16384^121 - 17) / 15
min6(X) :-
    X = biginteger.det_from_string(number11).

max6(X) :-
    X = biginteger.det_from_string(number12).

%%%
%%% integer variant
%%%

:- pred i_min1(integer::out) is det.
:- pred i_max1(integer::out) is det.

i_min1(X) :-
    X = integer.det_from_string(number1).

i_max1(X) :-
    X = integer.det_from_string(number2).

:- pred i_min2(integer::out) is det.
:- pred i_max2(integer::out) is det.

% 2^(16384*2) / 121
i_min2(X) :-
    X = integer.det_from_string(number3).

i_max2(X) :-
    X= integer.det_from_string(number4).


:- pred i_min3(integer::out) is det.
:- pred i_max3(integer::out) is det.

% (16384^100) / 333
i_min3(X) :-
    X = integer.det_from_string(number5).

i_max3(X) :-
    X = integer.det_from_string(number6).

:- pred i_min4(integer::out) is det.
:- pred i_max4(integer::out) is det.

% (16384^100) * 121 + 37
i_min4(X) :-
    X = integer.det_from_string(number7).

i_max4(X) :-
    X = integer.det_from_string(number8).

:- pred i_min5(integer::out) is det.
:- pred i_max5(integer::out) is det.

% 16384^121 / 13
i_min5(X) :-
    X = integer.det_from_string(number9).

i_max5(X) :-
    X = integer.det_from_string(number10).

:- pred i_min6(integer::out) is det.
:- pred i_max6(integer::out) is det.

% (16384^121 - 17) / 15
i_min6(X) :-
    X = integer.det_from_string(number11).

i_max6(X) :-
    X = integer.det_from_string(number12).


:- pragma memo(min1/1).
:- pragma memo(min2/1).
:- pragma memo(min3/1).
:- pragma memo(min4/1).
:- pragma memo(min5/1).
:- pragma memo(min6/1).

:- pragma memo(max1/1).
:- pragma memo(max2/1).
:- pragma memo(max3/1).
:- pragma memo(max4/1).
:- pragma memo(max5/1).
:- pragma memo(max6/1).


:- pragma memo(i_min1/1).
:- pragma memo(i_min2/1).
:- pragma memo(i_min3/1).
:- pragma memo(i_min4/1).
:- pragma memo(i_min5/1).
:- pragma memo(i_min6/1).

:- pragma memo(i_max1/1).
:- pragma memo(i_max2/1).
:- pragma memo(i_max3/1).
:- pragma memo(i_max4/1).
:- pragma memo(i_max5/1).
:- pragma memo(i_max6/1).


:- pred testBigintegerMinMaxMinMax(pred(biginteger), pred(biginteger),
                   pred(biginteger), pred(biginteger),
                   biginteger) is nondet.
:- mode testBigintegerMinMaxMinMax(pred(out) is det, pred(out) is det,
                            pred(out) is det, pred(out) is det, out).

testBigintegerMinMaxMinMax(PredMin1, PredMax1, PredMin2, PredMax2, Res) :-
    call(PredMin1, Min1),
    call(PredMax1, Max1),
    call(PredMin2, Min2),
    call(PredMax2, Max2),
    nondet_biginteger_in_range(Min1, Max1, Int1),
    nondet_biginteger_in_range(Min2, Max2, Int2),
    Res = Int1 * Int2.

:- pred testBigintegerMinMax(pred(biginteger), pred(biginteger),
                            biginteger) is nondet.
:- mode testBigintegerMinMax(pred(out) is det, pred(out) is det, out).

testBigintegerMinMax(PredMin, PredMax, Res) :-
    call(PredMin, Min),
    call(PredMax, Max),
    nondet_biginteger_in_range(Min, Max, Int1),
    nondet_biginteger_in_range(Int1, Max, Int2),
    Res = Int1 * Int2.


%
% integer variant
%
:- pred testIntegerMinMaxMinMax(pred(integer), pred(integer),
                   pred(integer), pred(integer),
                   integer) is nondet.
:- mode testIntegerMinMaxMinMax(pred(out) is det, pred(out) is det,
                               pred(out) is det, pred(out) is det, out).

testIntegerMinMaxMinMax(PredMin1, PredMax1, PredMin2, PredMax2, Res) :-
    call(PredMin1, Min1),
    call(PredMax1, Max1),
    call(PredMin2, Min2),
    call(PredMax2, Max2),
    nondet_integer_in_range(Min1, Max1, Int1),
    nondet_integer_in_range(Min2, Max2, Int2),
    Res = Int1 * Int2.

:- pred testIntegerMinMax(pred(integer), pred(integer), integer) is nondet.
:- mode testIntegerMinMax(pred(out) is det, pred(out) is det, out).

testIntegerMinMax(PredMin, PredMax, Res) :-
    call(PredMin, Min),
    call(PredMax, Max),
    nondet_integer_in_range(Min, Max, Int1),
    nondet_integer_in_range(Int1, Max, Int2),
    Res = Int1 * Int2.

:- pred nondet_integer_in_range(integer::in, integer::in, integer::out) is nondet.

nondet_integer_in_range(Min, Max, Res) :-
    (Min < Max ->
        ( Res = Min
        ;
          nondet_integer_in_range(Min + integer.one, Max, Res)
        )
    ;
        Min = Max,
        Res = Min
    ).


:- pred nondet_biginteger_in_range(biginteger::in, biginteger::in, biginteger::out) is nondet.

nondet_biginteger_in_range(Min, Max, Res) :-
    ( Min < Max ->
        ( Res = Min
        ;
            nondet_biginteger_in_range(Min + biginteger.one, Max, Res)
        )
    ;
        Min = Max,
        Res = Min
    ).

:- func integer_compare(biginteger,integer) = bool is semidet.

integer_compare(B1, I1) = Res :-
        biginteger.to_string(B1) = integer.to_string(I1),
        Res = yes.

:- pred testInteger1(integer::out) is nondet.
testInteger1(Res) :-
    testIntegerMinMax(i_min1, i_max1, Res).

:- pred testInteger2(integer::out) is nondet.
testInteger2(Res) :-
    testIntegerMinMaxMinMax(i_min1, i_max1, i_min2, i_max2, Res).

:- pred testInteger3(integer::out) is nondet.
testInteger3(Res) :-
    testIntegerMinMax(i_min3, i_max3, Res).

:- pred testInteger4(integer::out) is nondet.
testInteger4(Res) :-
    testIntegerMinMaxMinMax(i_min3, i_max3, i_min4, i_max4, Res).

:- pred testInteger5(integer::out) is nondet.
testInteger5(Res) :-
    testIntegerMinMax(i_min5, i_max5, Res).

:- pred testInteger6(integer::out) is nondet.
testInteger6(Res) :-
    testIntegerMinMaxMinMax(i_min5, i_max5, i_min6, i_max6, Res).

%
% variant
%

:- pred testBiginteger1(biginteger::out) is nondet.
testBiginteger1(Res) :-
    testBigintegerMinMax(min1, max1, Res).

:- pred testBiginteger2(biginteger::out) is nondet.
testBiginteger2(Res) :-
    testBigintegerMinMaxMinMax(min1, max1, min2, max2, Res).

:- pred testBiginteger3(biginteger::out) is nondet.
testBiginteger3(Res) :-
    testBigintegerMinMax(min3, max3, Res).

:- pred testBiginteger4(biginteger::out) is nondet.
testBiginteger4(Res) :-
    testBigintegerMinMaxMinMax(min3, max3, min4, max4, Res).

:- pred testBiginteger5(biginteger::out) is nondet.
testBiginteger5(Res) :-
    testBigintegerMinMax(min5, max5, Res).

:- pred testBiginteger6(biginteger::out) is nondet.
testBiginteger6(Res) :-
    testBigintegerMinMaxMinMax(min5, max5, min6, max6, Res).



% :- func makePredMinMax(pred(biginteger), pred(biginteger)) = pred(biginteger).
% :- mode makePredMinMax(in, in) = out.

% makePredMinMax(PredMin, PredMax) = testBigintegerMinMax(PredMax, PredMin).


main(!IO) :-
    %% execute biginteger tests
    (
      solutions(testBiginteger1, Liste1),
      solutions(testBiginteger2, Liste2),
      solutions(testBiginteger3, Liste3),
      solutions(testBiginteger4, Liste4),
      solutions(testBiginteger5, Liste5),
      solutions(testBiginteger6, Liste6)
    ),
    %% execute integer tests
    (
      solutions(testInteger1, IListe1),
      solutions(testInteger2, IListe2),
      solutions(testInteger3, IListe3),
      solutions(testInteger4, IListe4),
      solutions(testInteger5, IListe5),
      solutions(testInteger6, IListe6)
    ),
    (
      (
        list.filter_map_corresponding(integer_compare, Liste3, IListe3) = [],
        list.filter_map_corresponding(integer_compare, Liste4, IListe4) = [],
        list.filter_map_corresponding(integer_compare, Liste5, IListe5) = [],
        list.filter_map_corresponding(integer_compare, Liste6, IListe6) = []
      ) ->
        io.print("not the same", !IO)
    ;
        io.print("results are equal", !IO)
    ),
    nl(!IO).
