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

:- pragma foreign_type("C", mp_int, "mp_int*")
    where equality is mp_eq, comparison is mp_cmp.


:- import_module int.

:- pragma foreign_decl("C",
                      "#include \"tommath.h\"").

:- pred mp_init(int::in, mp_int::out) is det.

:- pragma foreign_proc("C",
                      mp_init(Value::in, Mp_Int::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      Mp_Int = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(Mp_Int);
                      mp_set_int(Mp_Int, Value);
                      ").

:- pragma foreign_proc("C",
                      mp_add(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      C = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(C);
                      mp_add(A, B, C);
                      ").

:- pragma foreign_proc("C",
                      mp_sub(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      C = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(C);
                      mp_sub(A, B, C);
                      ").

:- pragma foreign_proc("C",
                      mp_neg(A::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      C = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(C);
                      mp_neg(A, C);
                      ").

:- pragma foreign_proc("C",
                      mp_abs(A::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      C = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(C);
                      mp_abs(A, C);
                      ").

abs(A) = Res :- mp_abs(A, Res).

:- pragma foreign_proc("C",
                      mp_mul(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      C = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(C);
                      mp_mul(A, B, C);
                      ").

:- pragma foreign_proc("C",
                      mp_mul_2(A::in, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      B = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(B);
                      mp_mul_2(A, B);
                      ").

:- pragma foreign_proc("C",
                      mp_div_2(A::in, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      B = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(B);
                      mp_div_2(A, B);
                      ").

:- pragma foreign_proc("C",
                      mp_quot_rem(A::in, B::in, Quot::out, Rem::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      Quot = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      Rem = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(Quot);
                      mp_init(Rem);
                      mp_div(A, B, Quot, Rem);
                      ").

:- pragma foreign_proc("C",
                      mp_square(A::in, A_SQ::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      A_SQ = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(A_SQ);
                      mp_sqr(A, A_SQ);
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


:- pragma foreign_proc("C",
                      mp_to_string(A::in, Radix::in, S::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      int length;
                      mp_radix_size(A, Radix, &length);
                      MR_allocate_aligned_string_msg(S, length, MR_ALLOC_ID);
                      mp_toradix(A, S, Radix);
                      ").

to_string(A) = Res :- mp_to_string(A, 10, Res).

:- pragma foreign_proc("C",
                      mp_from_string(S::in, Radix::in, A::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
                      "
                      A = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
                      mp_init(A);
                      mp_read_radix(A, S, Radix);
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
