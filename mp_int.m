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
% To use the provided binding, one needs the compiled library and the .h
% include files, see README.txt for the details.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mp_int.

:- interface.

:- type mp_int.

    % Addition.
    %
:- func '+'(mp_int, mp_int) = mp_int.

    % Subtraction.
    %
:- func '-'(mp_int, mp_int) = mp_int.

    % Unary minus.
    %
:- func '-'(mp_int) = mp_int.

    % Multiplication.
    %
:- func '*'(mp_int, mp_int) = mp_int.

    % Truncating integer division, e.g., (-10) // 3 = (-3).
    %
:- func '//'(mp_int, mp_int) = mp_int.

    % Remainder.
    % X rem Y = X - (X // Y) * Y
    %
:- func 'rem'(mp_int, mp_int) = mp_int.

    % Absolute value.
    %
:- func abs(mp_int) = mp_int.

    % Squaring.
    %
:- func square(mp_int) = mp_int.

    % Truncating integer division with remainder.
    %
:- pred divide_with_rem(mp_int::in, mp_int::in, mp_int::out, mp_int::out)
    is det.

    % Multiplication by 2.
    %
:- pred multiply_by_2(mp_int::in, mp_int::out) is det.

    % Division by 2.
    %
:- pred divide_by_2(mp_int::in, mp_int::out) is det.

    % Shift Left.
    %
:- func '<<'(mp_int, int) = mp_int.

    % Shift Right.
    %
:- func '>>'(mp_int, int) = mp_int.

    % is_zero(X) if X is 0.
    %
:- pred is_zero(mp_int::in) is semidet.

    % is_even(X) if X is even.
    %
:- pred is_even(mp_int::in) is semidet.

    % is_odd(X) if X is odd.
    %
:- pred is_odd(mp_int::in) is semidet.

    % is_negative(X) if X is negative.
    %
:- pred is_negative(mp_int::in) is semidet.

    % Greater than.
    %
:- pred '>'(mp_int::in, mp_int::in) is semidet.

    % Less than.
    %
:- pred '<'(mp_int::in, mp_int::in) is semidet.

    % Greater or equal.
    %
:- pred '>='(mp_int::in, mp_int::in) is semidet.

    % Less or equal.
    %
:- pred '=<'(mp_int::in, mp_int::in) is semidet.

    % Equal.
    %
:- pred equal(mp_int::in, mp_int::in) is semidet.

    % Exponentiation.
    % Throws exception `math.domain_error` if Y is negative.
    %
:- func pow(mp_int, mp_int) = mp_int.

    % Convert mp_int to int.
    % Fails if not inside [min_int, max_int] interval.
    %
:- pred to_int(mp_int::in, int::out) is semidet.

    % As above, but throws exception if value is outside
    % [min_int, max_int] interval.
    %
:- func det_to_int(mp_int) = int.

    % to_base_string(Mp_Int, Base) = String:
    %
    % Convert mp_int to a string in given base.
    %
    % Base must be between 2 and 64, inclusive; if it is not, the predicate will
    % throw and exception.
    %
:- func to_base_string(mp_int, int) = string.

    % Convert mp_int to a string in base 10.
    %
:- func to_string(mp_int) = string.

    % from_base_string(String, Base, Mp_Int):
    %
    % Convert string in given base to mp_int.
    %
    % Base must be between 2 and 64, inclusive; fails if unsuccessful.
    %
:- pred from_base_string(string::in, int::in, mp_int::out) is semidet.
:- func from_base_string(string, int) = mp_int is semidet.

    % Convert string in base 10 to mp_int. Fails if unsuccesful.
    %
:- func from_string(string) = mp_int is semidet.

    % As above, throws exception instead of failing if unsuccessful.
    %
:- func det_from_string(string) = mp_int.

    % As above, throws exception instead of failing if unsuccessful.
    %
:- func det_from_base_string(string, int) = mp_int.

    % Convert an int to an mp_int.
    %
:- func mp_int(int) = mp_int.

    % Square root of mp_int.
    %
    % sqrt(X, Sqrt) is true if Sqrt is the positive square root of X.
    % Fails if X is negative.
    %
:- pred sqrt(mp_int::in, mp_int::out) is semidet.

    % As above, but throws error in case of negative value.
    %
:- func det_sqrt(mp_int) = mp_int.

    % Bitwise or.
    %
:- func mp_int \/ mp_int = mp_int.

    % Bitwise and.
    %
:- func mp_int /\ mp_int = mp_int.

    % Bitwise xor.
    %
:- func mp_int `xor` mp_int = mp_int.

    % Bitwise complement.
    %
:- func \ mp_int = mp_int.

    % Greatest common divisor.
    %
:- func gcd(mp_int, mp_int) = mp_int.

    % Least common multiple.
    %
:- func lcm(mp_int, mp_int) = mp_int.

    % jacobi(A, N) = C:
    %
    % Computes Jacobi symbol.
    %
    % C = J(A, N) = L(A, P_1)^(i_1) * ... * L(A, P_k)^(i_k) where
    %
    % A = P_1^(i_1) * ... * P_k^(i_k) with P_j is prime, and
    %
    %            / 1, if a is a quadratic residue modulo p, and a \= 0 (mod p)
    % L(A, P) = | -1, if a is a quadratic non-residue modulo p
    %            \ 0, if a is a multiple of p
    %
:- func jacobi(mp_int, mp_int) = int.

    % invmod(A, B) = C:
    %
    % Modular inverse C = A^(-1) mod B
    %
:- func invmod(mp_int, mp_int) = mp_int.

    % exptmod(A, B, C, D):
    %
    % Modular exponentiation D = A^B mod C.
    %
:- func exptmod(mp_int, mp_int, mp_int) = mp_int.

    % Probabilitic primality test.
    %
:- pred is_prime(mp_int::in) is semidet.

    % Probabilistic primality test with given number of rounds. Probability of
    % reporting composite number for a prime is ca. (1/4)^(-#Rounds)
    %
:- pred is_prime(mp_int::in, int::in) is semidet.

    % Constant 0.
    %
:- func zero = mp_int.

    % Constant 1.
    %
:- func one = mp_int.

    % Constant 2.
    %
:- func two = mp_int.

    % Constant -1.
    %
:- func negative_one = mp_int.

    % Constant 10.
    %
:- func ten = mp_int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module math.
:- import_module require.

%---------------------------------------------------------------------------%
% foreign declarations
%---------------------------------------------------------------------------%

   % Type declaration for foreign type mp_int*.
   %
:- pragma foreign_type("C", mp_int, "mp_int*")
    where equality is equal, comparison is mp_cmp.
:- pragma foreign_decl("C",
                      "#include \"tommath.h\"").

    % Result type to signal success or failure of external functions.
    %
:- type mp_result_type --->
      mp_result_okay
    ; mp_result_out_of_mem
    ; mp_result_invalid_input.

    % mapping of libtommath results to Mercury enum.
:- pragma foreign_enum("C", mp_result_type/0,
                      [
                       mp_result_okay          - "MP_OKAY",
                       mp_result_out_of_mem    - "MP_MEM",
                       mp_result_invalid_input - "MP_VAL"
                      ]).

:- type mp_bool_type --->
      mp_bool_yes
    ; mp_bool_no.

:- pragma foreign_enum("C", mp_bool_type/0,
                      [
                       mp_bool_yes - "MP_YES",
                       mp_bool_no  - "MP_NO"
                      ]).

%---------------------------------------------------------------------------%
% module internal predicate declarations
%---------------------------------------------------------------------------%

:- pred mp_init(int::in, mp_int::out) is det.
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

%---------------------------------------------------------------------------%
% basic arithmetic
%---------------------------------------------------------------------------%

:- pred mp_add(mp_int::in, mp_int::in, mp_int::out) is det.
mp_add(A, B, C) :-
    mp_add(A, B, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.mp_add: could not add"))
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

:- pred mp_sub(mp_int::in, mp_int::in, mp_int::out) is det.
mp_sub(A, B, C) :-
    mp_sub(A, B, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.mp_sub: could not subtract"))
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

:- pred mp_neg(mp_int::in, mp_int::out) is det.
mp_neg(A, C) :-
    mp_neg(A, Result, C0),
    ( Result = mp_result_okay ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.mp_neg: could not negate value"))
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

:- pred mp_abs(mp_int::in, mp_int::out) is det.
mp_abs(A, C) :-
    mp_abs(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error(
            "mp_int.mp_abs: could not compute absolute value"))
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

:- pred mp_mul(mp_int::in, mp_int::in, mp_int::out) is det.
mp_mul(A, B, C) :-
    mp_mul(A, B, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.mp_mul: could not multiply"))
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

multiply_by_2(A, C) :-
    mp_mul_2(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.multiply_by_2: could not double value"))
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

divide_by_2(A, C) :-
    mp_div_2(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.divide_by_2: could not halve value"))
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

divide_with_rem(A, B, Quot, Rem) :-
    ( is_zero(B) ->
        throw(math.domain_error("mp_int.quot_with_rem: division by is_zero"))
    ;
        mp_quot_rem(A, B, Result, Quot0, Rem0),
        (
          Result = mp_result_okay,
          Quot = Quot0,
          Rem = Rem0
        ;
          Result = mp_result_out_of_mem,
          error("could not initialize mp_int")
        ;
          Result = mp_result_invalid_input,
          throw(math.domain_error(
            "mp_int.quot_with_rem: could not compute quotient and remainder"))
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

rem(A, B) = Res :-
    ( is_zero(B) ->
        throw(math.domain_error("mp_int.rem: division by is_zero"))
    ;
        mp_rem(A, B, Result, Rem0),
        (
          Result = mp_result_okay,
          Res = Rem0
        ;
          Result = mp_result_out_of_mem,
          error("could not initialize mp_int")
        ;
          Result = mp_result_invalid_input,
          throw(math.domain_error(
                "mp_int.rem: could not compute remainder"))
        )
    ).

:- pred mp_rem(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_rem(A::in, B::in, Result::out, Rem::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  Rem        = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(Rem);
  if (initResult == MP_OKAY)
    {
      opResult = mp_div(A, B, NULL, Rem);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

:- func quotient(mp_int, mp_int) = mp_int.
quotient(A, B) = Res :-
    ( is_zero(B) ->
        throw(math.domain_error("mp_int.quotient: division by is_zero"))
    ;
        mp_quot(A, B, Result, Quot0),
        (
          Result = mp_result_okay,
          Res = Quot0
        ;
          Result = mp_result_out_of_mem,
          error("could not initialize mp_int")
        ;
          Result = mp_result_invalid_input,
          throw(math.domain_error(
                "mp_int.quotient: could not compute quotient"))
        )
    ).

:- pred mp_quot(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_quot(A::in, B::in, Result::out, Quot::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  Quot       = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(Quot);
  if (initResult == MP_OKAY)
    {
      opResult = mp_div(A, B, Quot, NULL);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

:- pred mp_square(mp_int::in, mp_int::out) is det.
mp_square(A, C) :-
    mp_square(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.mp_square: could not square"))
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

%---------------------------------------------------------------------------%
% conversion predicates
%---------------------------------------------------------------------------%

:- func min_int = mp_int.
min_int = mp_int(int.min_int).

:- func max_int = mp_int.
max_int = mp_int(int.max_int).

to_int(A, N) :-
    ( ( A >= min_int, A =< max_int) ->
        mp_to_long(A, N)
    ;
        fail
    ).

:- pred mp_to_long(mp_int::in, int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_to_long(A::in, N::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  unsigned long n;
  n = mp_get_long(A);
  N = (int) n;
").

det_to_int(A) = Res :-
    ( to_int(A, Res0) ->
        Res0 = Res
    ;
        throw(math.domain_error("mp_int.det_to_int: not in int range"))
    ).

to_base_string(A, Radix) = S :-
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

to_string(A) = to_base_string(A, 10).

from_base_string(S, Radix, A) :-
    mp_from_string(S, Radix, Result, A0),
    (
      Result = mp_result_okay,
      A = A0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      fail
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

from_base_string(S, R) = Res :- from_base_string(S, R, Res).

from_string(S) = Res :- from_base_string(S, 10, Res).

det_from_string(S) = Res :-
    ( from_base_string(S, 10, Res0) ->
        Res = Res0
    ;
        error("could not create mp_int from string")
    ).

det_from_base_string(S, Base) = Res :-
    ( from_base_string(S, Base, Res0) ->
        Res = Res0
    ;
        error("could not create mp_int from string")
    ).

mp_int(N) = Res :-
    mp_init(abs(N), Res0),
    ( N < 0 ->
        Res = -Res0
    ;
        Res = Res0
    ).

%---------------------------------------------------------------------------%
% bit shifting
%---------------------------------------------------------------------------%

A << N = Res :-
    mp_shift_left(A, N, Result, A0),
    (
      Result = mp_result_okay,
      Res = A0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.shift_left: could not shift"))
    ).

:- pred mp_shift_left(mp_int::in, int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_shift_left(A::in, N::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init_copy(B, A);
  if (initResult == MP_OKAY)
    {
      opResult = mp_lshd(B, N);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

A >> N = Res :-
    mp_shift_right(A, N, Result, A0),
    (
      Result = mp_result_okay,
      Res = A0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.shift_right: could not shift"))
    ).

:- pred mp_shift_right(mp_int::in, int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_shift_right(A::in, N::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init_copy(B, A);
  if (initResult == MP_OKAY)
    {
      mp_rshd(B, N);
      Result   = MP_OKAY;
    }
  else
    Result     = initResult;
").

%---------------------------------------------------------------------------%
% test predicates
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
                      is_zero(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mp_iszero(A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_even(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mp_iseven(A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_odd(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mp_isodd(A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_negative(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mp_isneg(A) ? MR_TRUE : MR_FALSE;
").

%---------------------------------------------------------------------------%
% comparison predicates
%---------------------------------------------------------------------------%

:- pred mp_cmp(comparison_result::uo, mp_int::in, mp_int::in) is det.

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

A > B :- mp_cmp((>), A, B).

A < B :- mp_cmp((<), A, B).

A >= B :-
    mp_cmp(C, A, B),
    ( C = (>); C = (=)).

A =< B :-
    mp_cmp(C, A, B),
    ( C = (<); C = (=)).

equal(A, B) :- mp_cmp((=), A, B).

%---------------------------------------------------------------------------%

A + B = C       :- mp_add(A, B, C).
A - B = C       :- mp_sub(A, B, C).
-A = C          :- mp_neg(A, C).
A * B = C       :- mp_mul(A, B, C).
A // B          = quotient(A, B).
square(X) = Res :- mp_square(X, Res).

%---------------------------------------------------------------------------%
% higher level functions
%---------------------------------------------------------------------------%

pow(A, N) = Res :-
    ( is_zero(N) ->
        Res = one
    ;
        ( is_even(N) ->
            Res = A * pow(A, N - one)
        ;
            divide_by_2(N, N0),
            SQ = pow(A, N0),
            mp_square(SQ, Res)
        )
    ).

%---------------------------------------------------------------------------%
% number theoretic functions
%---------------------------------------------------------------------------%

gcd(A, B) = Res :-
    mp_gcd(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      Res = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.gcd: could not compute gcd"))
    ).

:- pred mp_gcd(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_gcd(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_gcd(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

lcm(A, B) = Res :-
    mp_lcm(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      Res = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.lcm: could not compute lcm"))
    ).

:- pred mp_lcm(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_lcm(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_lcm(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

jacobi(A, P) = Res :-
    mp_jacobi(A, P, Result, C0),
    ( Result = mp_result_okay  ->
        Res = C0
    ;
        throw(math.domain_error(
              "mp_int.jacobi: could not compute Jacobi symbol of mp_int"))
    ).

:- pred mp_jacobi(mp_int::in, mp_int::in, mp_result_type::out, int::out) is det.
:- pragma foreign_proc("C",
                      mp_jacobi(A::in, P::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int opResult, res;
  opResult = mp_jacobi(A, P, &res);
  Result   = opResult;
  C        = res;
").

invmod(A, B) = Res :-
    mp_invmod(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      Res = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error(
            "mp_int.invmod: could not compute modular inverse"))
    ).

:- pred mp_invmod(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_invmod(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_invmod(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

exptmod(A, B, C) = Res :-
    mp_exptmod(A, B, C, Result, D0),
    (
      Result = mp_result_okay ,
      Res = D0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error(
            "mp_int.exptmod: could not compute modular exponentiation"))
    ).

:- pred mp_exptmod(mp_int::in, mp_int::in, mp_int::in, mp_result_type::out,
                  mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_exptmod(A::in, B::in, C::in, Result::out, D::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  D          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(D);
  if (initResult == MP_OKAY)
    {
      opResult = mp_exptmod(A, B, C, D);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

    % Default number of rounds for Miller-Rabin primality test.
    %
:- func miller_rabin_rounds = int.
miller_rabin_rounds = 40.

is_prime(A) :- is_prime(A, miller_rabin_rounds).

is_prime(A, Rounds) :-
    mp_is_prime(A, Rounds, Result, PResult),
    ( Result = mp_result_okay ->
        PResult = 1
    ;
        error("could not conduct Miller-Rabin primality test on mp_int")
    ).

:- pred mp_is_prime(mp_int::in, int::in, mp_result_type::out, int::out)
    is semidet.
:- pragma foreign_proc("C",
                      mp_is_prime(A::in, Rounds::in, Result::out, Value::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int opResult, result;
  opResult = mp_prime_is_prime(A, Rounds, &result);
  Value    = result;
  Result   = opResult;
").

sqrt(A, Res) :-
    ( is_negative(A) ->
        fail
    ;
        mp_sqrt(A, Result, C0),
        ( Result = mp_result_okay  ->
            Res = C0
        ;
            error("could not initialize mp_int")
        )
    ).

:- pred mp_sqrt(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_sqrt(A::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_sqrt(A, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

det_sqrt(A) = Res :-
    ( sqrt(A, Res0) ->
        Res = Res0
    ;
        throw(math.domain_error("mp_int.det_sqrt: argument negative"))
    ).

%---------------------------------------------------------------------------%
% bitwise operations
%---------------------------------------------------------------------------%

A /\ B = C :-
    mp_and(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int./\\: could not compute bitwise AND"))
    ).

:- pred mp_and(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_and(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_and(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

A \/ B = C :-
    mp_or(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.\\/: could not compute bitwise OR"))
    ).

:- pred mp_or(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_or(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_or(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

A `xor` B = C :-
    mp_xor(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(math.domain_error("mp_int.xor: could not compute bitwise XOR"))
    ).

:- pred mp_xor(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_xor(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int initResult, opResult;
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(C);
  if (initResult == MP_OKAY)
    {
      opResult = mp_xor(A, B, C);
      Result   = opResult;
    }
  else
    Result     = initResult;
").

\ X = Y :-
    mp_compl(X, Result, Y0),
    ( Result = mp_result_okay ->
        Y = Y0
    ;
        error("could not initialize mp_int")
    ).

:- pred mp_compl(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_compl(A::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int i, initResult, opResult;
  mp_digit tmpVal;
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  initResult = mp_init(B);
  if (initResult == MP_OKAY)
    {
      opResult = mp_copy(A, B);
      Result   = opResult;
      if (opResult == MP_OKAY)
        {
          for(i = 0; i < USED(A); i++)
            {
              tmpVal = B->dp[i];
              B->dp[i] = (~tmpVal & MP_MASK);
            }
        }
    }
  else
    Result     = initResult;
").

%---------------------------------------------------------------------------%
% often used constants
%---------------------------------------------------------------------------%

negative_one = mp_int(-1).
zero         = mp_int(0).
one          = mp_int(1).
two          = mp_int(2).
ten          = mp_int(10).

:- end_module mp_int.
