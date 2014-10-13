:- module psqueue_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module psqueue.

:- pred test_psqueue_empty is semidet.
test_psqueue_empty :-
    psqueue.init(PSQ),
    psqueue.is_empty(PSQ).

:- pred test_psqueue_singleton(psqueue(int, int)::out) is det.
test_psqueue_singleton(PSQ) :-
    psqueue.singleton(0, 10, PSQ).


:- pred test_psqueue_single_ltournament(psqueue(int, int)::out) is det.
test_psqueue_single_ltournament(PSQ) :-
    psqueue.singleton(1, 10, PSQ0),
    psqueue.singleton(0, 2, PSQ1),
    psqueue.tournament(PSQ0, PSQ1, PSQ).

:- pred test_psqueue_single_rtournament(psqueue(int, int)::out) is det.
test_psqueue_single_rtournament(PSQ) :-
    psqueue.singleton(1, 10, PSQ0),
    psqueue.singleton(0, 2, PSQ1),
    psqueue.tournament(PSQ1, PSQ0, PSQ).

:- pred test_psqueue_single_del_min(psqueue(int, int)::out) is semidet.
test_psqueue_single_del_min(PSQ) :-
    psqueue.singleton(1, 10, PSQ0),
    psqueue.singleton(0, 2, PSQ1),
    psqueue.tournament(PSQ1, PSQ0, PSQ2),
    del_min(PSQ2, _, _, PSQ).

:- pred test_psqueue_single_max_key(int::out, int::out) is semidet.
test_psqueue_single_max_key(MaxKeyl, MaxKeyr) :-
    psqueue.singleton(1, 10, PSQ0),
    psqueue.singleton(10, 2, PSQ1),
    psqueue.tournament(PSQ1, PSQ0, PSQ2),
    psqueue.tournament(PSQ0, PSQ1, PSQ3),
    psqueue.max_key(PSQ2, MaxKeyl),
    psqueue.max_key(PSQ3, MaxKeyr).

:- pred test_psqueue_paper_ex(psqueue(string, int)::out) is det.
test_psqueue_paper_ex(PSQ) :-
    psqueue.singleton("Lennart", 1, PSQ1),
    psqueue.singleton("Erik", 2, PSQ2),
    psqueue.singleton("Phil", 3, PSQ3),
    psqueue.singleton("Charles", 4, PSQ4),
    psqueue.singleton("Simon", 5, PSQ5),
    psqueue.singleton("Mary", 6, PSQ6),
    psqueue.singleton("Richard", 7, PSQ7),
    psqueue.singleton("Warren", 8, PSQ8),
    T0 = tournament(PSQ1,
                      tournament(PSQ2,
                                 tournament(PSQ3, PSQ4))),
    T1 = tournament(PSQ5,
                      tournament(PSQ6,
                                 tournament(PSQ7, PSQ8))),
    PSQ = tournament(T0, T1).

:- pred test_tournament_view_test(psqueue(K, P)::in, t_tournament_view(K, P)::out) is det.
test_tournament_view_test(PSQ, TV) :-
    TV = tournament_view(PSQ).

:- pred test_tournament_min_view(psqueue(K, P)::in, t_min_view(K, P)::out) is det.
test_tournament_min_view(PSQ, MV) :-
    MV = min_view(PSQ).

main(!IO) :-
    io.print("empty test: ", !IO),
    ( test_psqueue_empty ->
        io.print("ok", !IO)
    ;
        io.print("nok", !IO)
    ),
    io.nl(!IO),
    io.print("single tournament test: ", !IO),
    ( test_psqueue_singleton(PSQ),
        io.print("ok ", !IO),
        io.print(PSQ, !IO)
    ),
    io.nl(!IO),
    io.print("lloser test: ", !IO),
    ( test_psqueue_single_ltournament(PSQ0),
        io.print("ok ", !IO),
        io.print(PSQ0, !IO)
    ),
    io.nl(!IO),
    io.print("rloser test: ", !IO),
    ( test_psqueue_single_rtournament(PSQ1),
        io.print("ok ", !IO),
        io.print(PSQ1, !IO)
    ),
    io.nl(!IO),
    io.print("del_min test: ", !IO),
    ( test_psqueue_single_del_min(PSQ2) ->
        io.print("ok ", !IO),
        io.print(PSQ2, !IO)
    ;
        io.print("nok", !IO)
    ),
    io.nl(!IO),
    io.print("max_key test: ", !IO),
    ( test_psqueue_single_max_key(MaxKeyl, MaxKeyr) ->
        io.print("left: ", !IO),
        io.print(MaxKeyl, !IO),
        io.print(" right: ", !IO),
        io.print(MaxKeyr, !IO)
    ;
        io.print("nok", !IO)
    ),
    io.nl(!IO),
    io.print("paper example test: ", !IO),
    ( test_psqueue_paper_ex(PSQ_EX),
        io.print(PSQ_EX, !IO)
    ),
    io.nl(!IO),
    io.print("tournament view test: ", !IO),
    ( test_tournament_view_test(PSQ_EX, TV),
        io.print(TV, !IO)
    ),
    io.nl(!IO),
    io.print("min_view test: ", !IO),
    ( test_tournament_min_view(PSQ_EX, Min),
        io.print(Min, !IO)
    ),
    io.nl(!IO)
    .
