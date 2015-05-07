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

    % Absolute Value.
    %
:- func 'abs'(mp_int) = mp_int.

    % Multiplication.
    %
:- func '*'(mp_int, mp_int) = mp_int.

    % Truncating integer division, e.g., (-10) // 3 = (-3).
    %
:- func '//'(mp_int, mp_int) = mp_int.
:- func 'quotient'(mp_int, mp_int) = mp_int.

    % Remainder.
    % X rem Y = X - (X // Y) * Y
    %
:- func 'rem'(mp_int, mp_int) = mp_int.

    % Squaring.
    %
:- func square(mp_int) = mp_int.

    % Truncating integer division with remainder.
    %
:- pred quot_with_rem(mp_int::in, mp_int::in, mp_int::out, mp_int::out)
    is det.

    % Multiplication by 2.
    %
:- pred mp_mul_2(mp_int::in, mp_int::out) is det.

    % Division by 2.
    %
:- pred mp_div_2(mp_int::in, mp_int::out) is det.

    % Shift Left.
    %
:- func shift_left(mp_int, int) = mp_int.
:- func '<<'(mp_int, int) = mp_int.

    % Shift Right.
    %
:- func shift_right(mp_int, int) =  mp_int.
:- func '>>'(mp_int, int) = mp_int.

    % zero(X) if X is 0.
    %
:- pred zero(mp_int::in) is semidet.

    % even(X) if X is even.
    %
:- pred even(mp_int::in) is semidet.

    % odd(X) if X is odd.
    %
:- pred odd(mp_int::in) is semidet.

    % negative(X) if X is negative.
    %
:- pred negative(mp_int::in) is semidet.

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
:- pred mp_eq(mp_int::in, mp_int::in) is semidet.

    % Exponentiation.
    % Throws exception `math.domain_error` is Y is negative.
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

    % Convert mp_int to a string in given base.
    %
:- pred to_base_string(mp_int::in, int::in, string::out) is det.

    % Convert mp_int to a string in base 10.
    %
:- func to_string(mp_int) = string.

    % Convert string in given base to mp_int. Fails if unsuccesful.
    %
:- pred from_string(string::in, int::in, mp_int::out) is semidet.

    % Convert string in base 10 to mp_int. Fails if unsuccesful.
    %
:- func from_string(string) = mp_int is semidet.

    % As function above, exits on error/1 instead of failing.
    %
:- func det_from_string(string) = mp_int.

    % As function above, exits on error/1 instead of failing.
    %
:- func det_from_base_string(string, int) = mp_int.

    % Construct mp_int from int.
    %
:- func mp_int(int) = mp_int.

    % Greatest common divisor.
    %
:- func gcd(mp_int, mp_int) = mp_int.

    % Least common multiple.
    %
:- func lcm(mp_int, mp_int) = mp_int.

    % Jacobi symbol.
    %
:- func jacobi(mp_int, mp_int) = int.

    % Modular inverse C = A^(-1) mod B
    %
:- func invmod(mp_int, mp_int) = mp_int.

    % Modular exponentiation A = B^C mod D.
    %
:- func exptmod(mp_int, mp_int, mp_int) = mp_int.

    % Probabilitic primality test.
    %
:- pred is_prime(mp_int::in) is semidet.

    % Probabilistic primality test with given number of rounds. Probability of
    % reporting composite number for a prime is ca. (1/4)^(-#Rounds)
    %
:- pred is_prime(mp_int::in, int::in) is semidet.

    % Square root of mp_int.
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
:- func minusone = mp_int.

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
    where equality is mp_eq, comparison is mp_cmp.
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

:- pred mp_add(mp_int::in, mp_int::in, mp_int::out) is det.
:- pred mp_sub(mp_int::in, mp_int::in, mp_int::out) is det.
:- pred mp_neg(mp_int::in, mp_int::out) is det.
:- pred mp_abs(mp_int::in, mp_int::out) is det.
:- pred mp_mul(mp_int::in, mp_int::in, mp_int::out) is det.
:- pred mp_square(mp_int::in, mp_int::out) is det.

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

mp_add(A, B, C) :-
    mp_add(A, B, Result, C0),
    ( Result = mp_result_okay  ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.mp_add: could not add"))
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
            throw(math.domain_error("mp_int.mp_sub: could not subtract"))
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

mp_abs(A, C) :-
    mp_abs(A, Result, C0),
    ( Result = mp_result_okay ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.mp_abs: could not compute \
absolute value"))
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
            throw(math.domain_error("mp_int.mp_mul: could not multiply"))
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
            throw(math.domain_error("mp_int.mp_mul_2: could not double value"))
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
            throw(math.domain_error("mp_int.mp_div_2: could not halve value"))
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

quot_with_rem(A, B, Quot, Rem) :-
    ( zero(B) ->
        throw(math.domain_error("mp_int.quot_with_rem: division by zero"))
    ;
        mp_quot_rem(A, B, Result, Quot0, Rem0),
        ( Result = mp_result_okay ->
            Quot = Quot0,
            Rem = Rem0
        ;
            ( Result = mp_result_out_of_mem ->
                error("could not initialize mp_int")
            ;
                throw(math.domain_error("mp_int.quot_with_rem: could not \
compute quotient and remainder"))
            )
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
    ( zero(B) ->
        throw(math.domain_error("mp_int.rem: division by zero"))
    ;
        mp_rem(A, B, Result, Rem0),
        ( Result = mp_result_okay ->
            Res = Rem0
        ;
            ( Result = mp_result_out_of_mem ->
                error("could not initialize mp_int")
            ;
                throw(math.domain_error("mp_int.rem: could not \
compute remainder"))
            )
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

quotient(A, B) = Res :-
    ( zero(B) ->
        throw(math.domain_error("mp_int.quotient: division by zero"))
    ;
        mp_quot(A, B, Result, Quot0),
        ( Result = mp_result_okay ->
            Res = Quot0
        ;
            ( Result = mp_result_out_of_mem ->
                error("could not initialize mp_int")
            ;
                throw(math.domain_error("mp_int.quotient: could not \
compute quotient"))
            )
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

mp_square(A, C) :-
    mp_square(A, Result, C0),
    ( Result = mp_result_okay ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.mp_square: could not square"))
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
  N = n;
").

det_to_int(A) = Res :-
    ( to_int(A, Res0) ->
        Res0 = Res
    ;
        throw(math.domain_error("mp_int.det_to_int: not in int range"))
    ).

to_base_string(A, Radix, S) :-
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

to_string(A) = Res :- to_base_string(A, 10, Res).

from_string(S, Radix, A) :-
    mp_from_string(S, Radix, Result, A0),
    ( Result = mp_result_okay ->
        A = A0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            fail
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

from_string(S) = Res :- from_string(S, 10, Res).

det_from_string(S) = Res :-
    ( from_string(S, 10, Res0) ->
        Res = Res0
    ;
        error("could not create mp_int from string")
    ).

det_from_base_string(S, Base) = Res :-
    ( from_string(S, Base, Res0) ->
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

shift_left(A, N) = Res :-
    mp_shift_left(A, N, Result, A0),
    ( Result = mp_result_okay ->
        Res = A0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.shift_left: could not shift"))
        )
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

shift_right(A, N) = Res :-
    mp_shift_right(A, N, Result, A0),
    ( Result = mp_result_okay ->
        Res = A0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.shift_right: could not shift"))
        )
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

X << N = shift_left(X, N).

X >> N = shift_right(X, N).

%---------------------------------------------------------------------------%
% test predicates
%---------------------------------------------------------------------------%

zero(A) :-
    mp_iszero(A, Result),
    Result = mp_bool_yes.

:- pred mp_iszero(mp_int::in, mp_bool_type::out) is det.
:- pragma foreign_proc("C",
                      mp_iszero(A::in, Result::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int opResult;
  opResult = mp_iszero(A);
  Result   = opResult;
").

even(A) :-
    mp_iseven(A, Result),
    Result = mp_bool_yes.

:- pred mp_iseven(mp_int::in, mp_bool_type::out) is det.
:- pragma foreign_proc("C",
                      mp_iseven(A::in, Result::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int opResult;
  opResult = mp_iseven(A);
  Result   = opResult;
").

odd(A) :-
    mp_isodd(A, Result),
    Result = mp_bool_yes.

:- pred mp_isodd(mp_int::in, mp_bool_type::out) is det.
:- pragma foreign_proc("C",
                      mp_isodd(A::in, Result::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int opResult;
  opResult = mp_isodd(A);
  Result   = opResult;
").

negative(A) :-
    mp_isneg(A, Result),
    Result = mp_bool_yes.

:- pred mp_isneg(mp_int::in, mp_bool_type::out) is det.
:- pragma foreign_proc("C",
                      mp_isneg(A::in, Result::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int opResult;
  opResult = mp_isneg(A);
  Result   = opResult;
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

mp_eq(A, B) :- mp_cmp((=), A, B).

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
    ( zero(N) ->
        Res = one
    ;
        ( N rem two = one ->
            Res = A * pow(A, N - one)
        ;
            mp_div_2(N, N0),
            SQ = pow(A, N0),
            mp_square(SQ, Res)
        )
    ).

%---------------------------------------------------------------------------%
% number theoretic functions
%---------------------------------------------------------------------------%

gcd(A, B) = Res :-
    mp_gcd(A, B, Result, C0),
    ( Result = mp_result_okay  ->
        Res = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.gcd: could not compute gcd"))
        )
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
    ( Result = mp_result_okay  ->
        Res = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.lcm: could not compute lcm"))
        )
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
        throw(math.domain_error("mp_int.jacobi: could not compute Jacobi \
symbol of mp_int"))
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
    ( Result = mp_result_okay  ->
        Res = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.invmod: could not compute\
modular inverse"))
        )
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
    ( Result = mp_result_okay  ->
        Res = D0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.exptmod: could not compute modular \
exponentiation"))
        )
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
    ( A >= zero ->
        mp_sqrt(A, Result, C0),
        ( Result = mp_result_okay  ->
            Res = C0
        ;
            error("could not initialize mp_int")
        )
    ;
        fail
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
    ( Result = mp_result_okay  ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int./\\: could not compute bitwise \
AND"))
        )
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
    ( Result = mp_result_okay  ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.\\/: could not compute bitwise \
OR"))
        )
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
    ( Result = mp_result_okay  ->
        C = C0
    ;
        ( Result = mp_result_out_of_mem ->
            error("could not initialize mp_int")
        ;
            throw(math.domain_error("mp_int.xor: could not compute bitwise \
XOR"))
        )
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

%---------------------------------------------------------------------------%
% often used constants
%---------------------------------------------------------------------------%

minusone = mp_int(-1).
zero     = mp_int(0).
one      = mp_int(1).
two      = mp_int(2).
ten      = mp_int(10).

:- end_module mp_int.
