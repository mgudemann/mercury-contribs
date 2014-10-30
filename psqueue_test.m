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

% :- pred test_psqueue_singleton(psqueue(int, int)::out) is det.
% test_psqueue_singleton(PSQ) :-
%     psqueue.singleton(0, 10, PSQ).


% :- pred test_psqueue_single_ltournament(psqueue(int, int)::out) is det.
% test_psqueue_single_ltournament(PSQ) :-
%     psqueue.singleton(1, 10, PSQ0),
%     psqueue.singleton(0, 2, PSQ1),
%     psqueue.tournament(PSQ0, PSQ1, PSQ).

% :- pred test_psqueue_single_rtournament(psqueue(int, int)::out) is det.
% test_psqueue_single_rtournament(PSQ) :-
%     psqueue.singleton(1, 10, PSQ0),
%     psqueue.singleton(0, 2, PSQ1),
%     psqueue.tournament(PSQ1, PSQ0, PSQ).

% :- pred test_psqueue_single_del_min(psqueue(int, int)::out) is semidet.
% test_psqueue_single_del_min(PSQ) :-
%     psqueue.singleton(1, 10, PSQ0),
%     psqueue.singleton(0, 2, PSQ1),
%     psqueue.tournament(PSQ1, PSQ0, PSQ2),
%     del_min(PSQ2, _, _, PSQ).

% :- pred test_psqueue_single_max_key(int::out, int::out) is semidet.
% test_psqueue_single_max_key(MaxKeyl, MaxKeyr) :-
%     psqueue.singleton(1, 10, PSQ0),
%     psqueue.singleton(10, 2, PSQ1),
%     psqueue.tournament(PSQ1, PSQ0, PSQ2),
%     psqueue.tournament(PSQ0, PSQ1, PSQ3),
%     psqueue.max_key(PSQ2, MaxKeyl),
%     psqueue.max_key(PSQ3, MaxKeyr).

:- pred test_psqueue_paper_ex(psqueue(string, int)::out) is det.
test_psqueue_paper_ex(PSQ) :-
    PSQ1 = psqueue.insert("Warren", 8, psqueue.init),
    PSQ2 = psqueue.insert("Erik", 2, PSQ1),
    PSQ3 = psqueue.insert("Richard", 7, PSQ2),
    PSQ4 = psqueue.insert("Simon", 5, PSQ3),
    PSQ5 = psqueue.insert("Charles", 4, PSQ4),
    PSQ6 = psqueue.insert("Mary", 6, PSQ5),
    PSQ7 = psqueue.insert("Phil", 3, PSQ6),
    PSQ8 = psqueue.insert("Lennart", 1, PSQ7),
    % T0 = tournament(PSQ1,
    %                   tournament(PSQ2,
    %                              tournament(PSQ3, PSQ4))),
    % T1 = tournament(PSQ5,
    %                   tournament(PSQ6,
    %                              tournament(PSQ7, PSQ8))),
    % PSQ = tournament(T0, T1).
    PSQ = PSQ8.

% :- pred test_tournament_view_test(psqueue(K, P)::in, t_tournament_view(K, P)::out) is det.
% test_tournament_view_test(PSQ, TV) :-
%     TV = tournament_view(PSQ).

% :- pred test_tournament_min_view(psqueue(K, P)::in, t_min_view(K, P)::out) is det.
% test_tournament_min_view(PSQ, MV) :-
%     MV = min_view(PSQ).

main(!IO) :-
    io.print("empty test: ", !IO),
    ( test_psqueue_empty ->
        io.print("ok", !IO)
    ;
        io.print("nok", !IO)
    ),
    io.nl(!IO),
    io.print("paper example test: ", !IO),
    ( test_psqueue_paper_ex(PSQ_EX),
        io.print(PSQ_EX, !IO)
    ),
    io.nl(!IO),
    io.print("to_ord_assoc_list test: ", !IO),
    ( to_ord_assoc_list(PSQ_EX, AList),
        io.print(AList, !IO)
    ),
    io.nl(!IO),
    io.print("delete and to_or_assoc: ", !IO),
    ( delete("Lennart", PSQ_EX, PSQ_DEL),
        io.print(PSQ_DEL, !IO),
        io.nl(!IO),
        to_ord_assoc_list(PSQ_DEL, AList0),
        io.print(AList0, !IO)
    ),
    io.nl(!IO),
    io.print("from_assoc_list: ", !IO),
    (
      init(PSQ0),
      insert("Homer", 4, PSQ0, PSQ1),
      insert("Lisa", 1, PSQ1, PSQ2),
      insert("Bart", 2, PSQ2, PSQ3),
      insert("Maggie", 0, PSQ3, PSQ4),
      insert("Marge", 3, PSQ4, PSQ5),
      io.print(PSQ5, !IO),
      io.nl(!IO),
      to_ord_assoc_list(PSQ5, AList1),
      io.print(AList1, !IO)
    ),
    io.nl(!IO),
    (
      adjust(func(_) = 10, "Maggie", PSQ5, PSQ6),
      to_ord_assoc_list(PSQ6, AList2),
      io.print(AList2, !IO)
    ),
    io.nl(!IO),
    (
      at_most(4, PSQ_EX, AList3),
      io.print(AList3, !IO)
    ),
    io.nl(!IO)
    .
