%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% File: mp_int.m.
% Main author: Matthias Güdemann.
% Stability: low.
%
% This module implements a binding to libtomath.
%
% This library provides a portable ISO C implementation of multi precision
% integers. libtommath is in the public domain and its source code is available
% from https://github.com/libtom/libtommath.
%
% To use the provided binding, one needs the compiled library and the .h include
% files.
%
% ---------------------------------------------------------------------------%
% ---------------------------------------------------------------------------%

:- module mp_int.

:- interface.

:- type mp_int.

:- pred mp_add(mp_int::in, mp_int::in, mp_int::out) is det.
:- pred mp_sub(mp_int::in, mp_int::in, mp_int::out) is det.
:- pred mp_neg(mp_int::in, mp_int::out) is det.
:- pred mp_abs(mp_int::in, mp_int::out) is det.
:- pred mp_mul(mp_int::in, mp_int::in, mp_int::out) is det.
:- pred mp_quot_rem(mp_int::in, mp_int::in, mp_int::out, mp_int::out) is det.
:- pred mp_square(mp_int::in, mp_int::out) is det.

:- func '+'(mp_int, mp_int) = mp_int.
:- func '-'(mp_int, mp_int) = mp_int.
:- func '-'(mp_int) = mp_int.
:- func 'abs'(mp_int) = mp_int.
:- func '*'(mp_int, mp_int) = mp_int.
:- func '//'(mp_int, mp_int) = mp_int.
:- func 'div'(mp_int, mp_int) = mp_int.
:- func 'mod'(mp_int, mp_int) = mp_int.

:- func zero = mp_int.
:- func one = mp_int.
:- func two = mp_int.

:- pred mp_mul_2(mp_int::in, mp_int::out) is det.
:- pred mp_div_2(mp_int::in, mp_int::out) is det.
:- func mp_shift_left(mp_int, int) = mp_int.
:- func mp_shift_right(mp_int, int) =  mp_int.
:- func '<<'(mp_int, int) = mp_int.
:- func '>>'(mp_int, int) = mp_int.

:- pred '>'(mp_int::in, mp_int::in) is semidet.
:- pred '<'(mp_int::in, mp_int::in) is semidet.
:- pred '>='(mp_int::in, mp_int::in) is semidet.
:- pred '=<'(mp_int::in, mp_int::in) is semidet.
:- pred mp_eq(mp_int::in, mp_int::in) is semidet.

:- pred mp_cmp(comparison_result::uo, mp_int::in, mp_int::in) is det.

:- func pow(mp_int, mp_int) = mp_int.

:- pred mp_to_string(mp_int::in, int::in, string::out) is det.
:- func to_string(mp_int) = string.

:- pred mp_from_string(string::in, int::in, mp_int::out) is det.
:- func from_string(string) = mp_int.
:- func mp_int(int) = mp_int.

:- implementation.

:- import_module int, require.

% type declaration for foreig type mp_int*
:- pragma foreign_type("C", mp_int, "mp_int*")
    where equality is mp_eq, comparison is mp_cmp.

:- pragma foreign_decl("C",
                      "#include \"tommath.h\"").

:- pred mp_init(int::in, mp_int::out) is det.

% result type to signal success or failure of external functions
:- type mp_result_type == int.

:- func mp_result_okay = mp_result_type.
:- func mp_result_mem = mp_result_type.
:- func mp_result_val = mp_result_type.

mp_result_okay = 0.
mp_result_mem = -2.
mp_result_val = -3.

% all external functions are deterministic, they abort
% if either initialization of a variable or the operation itself fails

mp_init(N, Res) :-
    mp_init(N, InitResult, OpResult, Res0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay ) ->
        Res = Res0
    ;
        error("could not initialize mp_int")
    ).

:- pred mp_init(int::in, mp_result_type::out, mp_result_type::out,
               mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_init(Value::in, InitResult::out, OpResult::out,
                              Mp_Int::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      Mp_Int     = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(Mp_Int);
                      opResult   = mp_set_int(Mp_Int, Value);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

mp_add(A, B, C) :-
    mp_add(A, B, InitResult, OpResult, C0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        C = C0
    ;
        error("could not add mp_ints")
    ).

:- pred mp_add(mp_int::in, mp_int::in, mp_result_type::out, mp_result_type::out,
              mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_add(A::in, B::in, InitResult::out, OpResult::out,
                             C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(C);
                      opResult   = mp_add(A, B, C);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

mp_sub(A, B, C) :-
    mp_sub(A, B, InitResult, OpResult, C0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        C = C0
    ;
        error("could not subtract mp_ints")
    ).

:- pred mp_sub(mp_int::in, mp_int::in, mp_result_type::out, mp_result_type::out,
              mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_sub(A::in, B::in, InitResult::out, OpResult::out,
                             C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(C);
                      opResult   = mp_sub(A, B, C);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

mp_neg(A, C) :-
    mp_neg(A, InitResult, OpResult, C0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        C = C0
    ;
        error("could not negate mp_int")
    ).

:- pred mp_neg(mp_int::in, mp_result_type::out, mp_result_type::out,
              mp_int::out)is det.
:- pragma foreign_proc("C",
                      mp_neg(A::in, InitResult::out, OpResult::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(C);
                      opResult   = mp_neg(A, C);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

mp_abs(A, C) :-
    mp_abs(A, InitResult, OpResult, C0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        C = C0
    ;
        unexpected($file, $pred,
                   "could compute absolute value of mp_int")
    ).

:- pred mp_abs(mp_int::in, mp_result_type::out, mp_result_type::out,
              mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_abs(A::in, InitResult::out, OpResult::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(C);
                      opResult   = mp_abs(A, C);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

abs(A) = Res :- mp_abs(A, Res).

mp_mul(A, B, C) :-
    mp_mul(A, B, InitResult, OpResult, C0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        C = C0
    ;
        error("could not multiply mp_ints")
    ).

:- pred mp_mul(mp_int::in, mp_int::in, mp_result_type::out, mp_result_type::out,
              mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_mul(A::in, B::in, InitResult::out, OpResult::out,
                             C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(C);
                      opResult   = mp_mul(A, B, C);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

mp_mul_2(A, C) :-
    mp_mul_2(A, InitResult, OpResult, C0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        C = C0
    ;
        error("could not double mp_int")
    ).

:- pred mp_mul_2(mp_int::in, mp_result_type::out, mp_result_type::out,
                mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_mul_2(A::in, InitResult::out, OpResult::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(B);
                      opResult   = mp_mul_2(A, B);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

mp_div_2(A, C) :-
    mp_div_2(A, InitResult, OpResult, C0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        C = C0
    ;
        error("could not halve mp_int")
    ).

:- pred mp_div_2(mp_int::in, mp_result_type::out, mp_result_type::out,
                mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_div_2(A::in, InitResult::out, OpResult::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(B);
                      opResult   = mp_div_2(A, B);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

mp_quot_rem(A, B, Quot, Rem) :-
    mp_quot_rem(A, B, InitResult1, InitResult2, OpResult, Quot0, Rem0),
    ( ( InitResult1 = mp_result_okay, InitResult2 = mp_result_okay,
          OpResult = mp_result_okay) ->
        Quot = Quot0,
        Rem = Rem0
    ;
        unexpected($file, $pred,
          "could not compute quotient and remainder of mp_ints")
    ).

:- pred mp_quot_rem(mp_int::in, mp_int::in, mp_result_type::out,
                   mp_result_type::out, mp_result_type::out, mp_int::out,
                   mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_quot_rem(A::in, B::in, InitResult1::out,
                                  InitResult2::out, OpResult::out, Quot::out,
                                  Rem::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult1, initResult2, opResult;
                      Quot        = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      Rem         = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult1 = mp_init(Quot);
                      initResult2 = mp_init(Rem);
                      opResult    = mp_div(A, B, Quot, Rem);
                      InitResult1 = initResult1;
                      InitResult2 = initResult2;
                      OpResult    = opResult;
                      ").

mp_square(A, C) :-
    mp_square(A, InitResult, OpResult, C0),
    ( ( InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        C = C0
    ;
        error("could not square mp_int")
    ).

:- pred mp_square(mp_int::in, mp_result_type::out, mp_result_type::out,
                 mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_square(A::in, InitResult::out, OpResult::out,
                                A_SQ::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      A_SQ       = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(A_SQ);
                      opResult   = mp_sqr(A, A_SQ);
                      InitResult = initResult;
                      OpResult   = opResult;
                      ").

:- pragma foreign_proc("C",
                      mp_cmp(C::uo, A::in, B::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int result;
                      result = mp_cmp(A, B);
                      if (result == MP_LT)
                         C = MR_COMPARE_LESS;
                      else
                       {
                         if (result == MP_GT)
                            C = MR_COMPARE_GREATER;
                         else
                            C = MR_COMPARE_EQUAL;
                       }
                      ").

mp_to_string(A, Radix, S) :-
    mp_to_string(A, Radix, OpResult1, OpResult2, S0),
    ( ( OpResult1 = mp_result_okay, OpResult2 = mp_result_okay ) ->
        S = S0
    ;
        error("could not convert mp_int to string")
    ).

:- pred mp_to_string(mp_int::in, int::in, mp_result_type::out,
                    mp_result_type::out, string::out) is det.
:- pragma foreign_proc("C",
                      mp_to_string(A::in, Radix::in, OpResult1::out,
                                   OpResult2::out,S::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int length, opResult1, opResult2;
                      opResult1 = mp_radix_size(A, Radix, &length);
                      MR_allocate_aligned_string_msg(S, length, MR_ALLOC_ID);
                      opResult2 = mp_toradix(A, S, Radix);
                      OpResult1  = opResult1;
                      OpResult2  = opResult2;
                      ").

to_string(A) = Res :- mp_to_string(A, 10, Res).

mp_from_string(S, Radix, A) :-
    mp_from_string(S, Radix, InitResult, OpResult, A0),
    ( (InitResult = mp_result_okay, OpResult = mp_result_okay) ->
        A = A0
    ;
        error("could not convert string to mp_int")
    ).

:- pred mp_from_string(string::in, int::in, mp_result_type::out,
                      mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_from_string(S::in, Radix::in, InitResult::out,
                                     OpResult::out, A::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int initResult, opResult;
                      A = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      initResult = mp_init(A);
                      opResult   = mp_read_radix(A, S, Radix);
                      OpResult   = opResult;
                      InitResult = initResult;
                      ").

from_string(S) = Res :- mp_from_string(S, 10, Res).

mp_int(N) = Res :- mp_init(N, Res).

mp_shift_left(A, N) = Res :-
    ( N > 0 ->
        mp_mul_2(A, B),
        Res = mp_shift_left(B, N - 1)
    ;
        Res = A
    ).

X << N = mp_shift_left(X, N).

mp_shift_right(A, N) = Res :-
    ( N > 0 ->
        mp_div_2(A, B),
        Res = mp_shift_right(B, N - 1)
    ;
        Res = A
    ).

X >> N = mp_shift_right(X, N).

A > B :- mp_cmp((>), A, B).

A < B :- mp_cmp((<), A, B).

A >= B :-
    mp_cmp(C, A, B),
    ( C = (>); C = (=)).

A =< B :-
    mp_cmp(C, A, B),
    ( C = (<); C = (=)).

mp_eq(A, B) :- mp_cmp((=), A, B).

A + B = C   :- mp_add(A, B, C).
A - B = C   :- mp_sub(A, B, C).
-A = C      :- mp_neg(A, C).
A * B = C   :- mp_mul(A, B, C).
A // B = C  :- mp_quot_rem(A, B, C, _).
A div B     = A // B.
A mod B = C :- mp_quot_rem(A, B, _, C).

zero = mp_int(0).
one  = mp_int(1).
two  = mp_int(2).

pow(A, N) = Res :-
    ( N = zero ->
        Res = one
    ;
        ( N mod two = one ->
            Res = A * pow(A, N - one)
        ;
            SQ = pow(A, N // two),
            mp_square(SQ, Res)
        )
    ).
