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
:- type mp_result_type --->
      mp_result_okay
    ; mp_result_out_of_mem
    ; mp_result_invalid_input.

:- pragma foreign_enum("C", mp_result_type/0,
                      [mp_result_okay          - "MP_OKAY",
                       mp_result_out_of_mem    - "MP_MEM",
                       mp_result_invalid_input - "MP_VAL"
                      ]).

% all external functions are deterministic, they abort
% if either initialization of a variable or the operation itself fails

mp_init(N, Res) :-
    mp_init(N, Result, Res0),
    ( Result = mp_result_okay ->
        Res = Res0
    ;
        error("could not initialize mp_int")
    ).

:- pred mp_init(int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_init(Value::in, Result::out, Mp_Int::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  Mp_Int     = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(Mp_Int);
  if (initResult == MP_OKAY)
    {
      opResult = mp_set_int(Mp_Int, Value);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

mp_add(A, B, C) :-
    mp_add(A, B, Result, C0),
    ( Result = mp_result_okay  ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not add mp_ints")
        )
    ).

:- pred mp_add(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_add(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_add(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

mp_sub(A, B, C) :-
    mp_sub(A, B, Result, C0),
    ( Result = mp_result_okay  ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not subtract mp_ints")
        )
    ).

:- pred mp_sub(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_sub(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_sub(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

mp_neg(A, C) :-
    mp_neg(A, Result, C0),
    ( Result = mp_result_okay ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not negate mp_int")
        )
    ).

:- pred mp_neg(mp_int::in, mp_result_type::out, mp_int::out)is det.
:- pragma foreign_proc("C",
                      mp_neg(A::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_neg(A, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

mp_abs(A, C) :-
    mp_abs(A, Result, C0),
    ( Result = mp_result_okay ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could compute absolute value of mp_int")
        )
    ).

:- pred mp_abs(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_abs(A::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_abs(A, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

abs(A) = Res :- mp_abs(A, Res).

mp_mul(A, B, C) :-
    mp_mul(A, B, Result, C0),
    ( Result = mp_result_okay  ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not multiply mp_ints")
        )
    ).

:- pred mp_mul(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_mul(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_mul(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

mp_mul_2(A, C) :-
    mp_mul_2(A, Result, C0),
    ( Result = mp_result_okay ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not double mp_int")
        )
    ).

:- pred mp_mul_2(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_mul_2(A::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(B);
  if (initResult == MP_OKAY)
    {
      opResult = mp_mul_2(A, B);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

mp_div_2(A, C) :-
    mp_div_2(A, Result, C0),
    ( Result = mp_result_okay ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not halve mp_int")
        )
    ).

:- pred mp_div_2(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_div_2(A::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(B);
  if (initResult == MP_OKAY)
    {
      opResult = mp_div_2(A, B);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

mp_quot_rem(A, B, Quot, Rem) :-
    mp_quot_rem(A, B, Result, Quot0, Rem0),
    ( Result = mp_result_okay ->
        Quot = Quot0,
        Rem = Rem0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not compute quotient and remainder of mp_ints")
        )
    ).

:- pred mp_quot_rem(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out,
                   mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_quot_rem(A::in, B::in, Result::out,
                                  Quot::out, Rem::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult1, initResult2, opResult;
  Quot        = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Rem         = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult1 = mp_init(Quot);
  initResult2 = mp_init(Rem);
  if (initResult1 == MP_OKAY && initResult2 == MP_OKAY)
    {
      opResult = mp_div(A, B, Quot, Rem);
      Result   = opResult;
    }
  else
    Result     = initResult1 != MP_OKAY ? initResult1 : initResult2;
").

mp_square(A, C) :-
    mp_square(A, Result, C0),
    ( Result = mp_result_okay ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not square mp_int")
        )
    ).

:- pred mp_square(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_square(A::in, Result::out, A_SQ::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  A_SQ       = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(A_SQ);
  if (initResult == MP_OKAY)
    {
      opResult = mp_sqr(A, A_SQ);
      Result   = opResult;
    }
  else
    Result     = initResult;
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
    mp_to_string(A, Radix, Result, S0),
    ( Result = mp_result_okay ->
        S = S0
    ;
        error("could not convert mp_int to string")
    ).

:- pred mp_to_string(mp_int::in, int::in, mp_result_type::out, string::out)
    is det.
:- pragma foreign_proc("C",
                      mp_to_string(A::in, Radix::in, Result::out, S::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int length, opResult;
  opResult = mp_radix_size(A, Radix, &length);
  if (opResult == MP_OKAY)
    {
      MR_allocate_aligned_string_msg(S, length, MR_ALLOC_ID);
      opResult = mp_toradix(A, S, Radix);
      Result   = opResult;
    }
  else
    Result     = opResult;
").

to_string(A) = Res :- mp_to_string(A, 10, Res).

mp_from_string(S, Radix, A) :-
    mp_from_string(S, Radix, Result, A0),
    ( Result = mp_result_okay ->
        A = A0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            error("could not convert string to mp_int")
        )
    ).

:- pred mp_from_string(string::in, int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_from_string(S::in, Radix::in, Result::out, A::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  A          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(A);
  if (initResult == MP_OKAY)
    {
      opResult = mp_read_radix(A, S, Radix);
      Result   = opResult;
    }
  else
    Result     = initResult;
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
            mp_div_2(N, N0),
            SQ = pow(A, N0),
            mp_square(SQ, Res)
        )
    ).
