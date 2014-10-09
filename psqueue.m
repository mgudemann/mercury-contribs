:- module psqueue.
:- interface.

:- type psqueue(K, P).

:- func psqueue.init = psqueue(K, P).
:- pred psqueue.init(psqueue(K, P)::out) is det.

:- pred psqueue.is_empty(psqueue(K, P)::in) is semidet.

:- pred psqueue.singleton(K::in, P::in, psqueue(K, P)::out) is det.
:- func psqueue.singleton(K, P) = psqueue(K, P).

:- func psqueue.max_key(psqueue(K, P)) = K is semidet.
:- pred psqueue.max_key(psqueue(K, P)::in, K::out) is semidet.

:- implementation.

:- type psqueue(K, P) --->
    void
    ; winner(K, P, ltree(K, P), K).

:- type ltree(K, P) --->
    start
    ; lloser(K, P, ltree(K, P), K, ltree(K, P))
    ; rloser(K, P, ltree(K, P), K, ltree(K, P)).


% create empty psqueue
psqueue.init = PSQ :-
        psqueue.init(PSQ).

psqueue.init(void).


% check for empty psqueue
psqueue.is_empty(void).


% create singleton psqueue
singleton(K, P) = Res :-
    singleton(K, P, Res).

singleton(K, P, PSQ) :-
    PSQ = winner(K, P, start, K).

% extract maximal (highest priority) key
max_key(PSQ) = K :-
        max_key(PSQ, K).

max_key(PSQ, MaxKey) :-
        PSQ = winner(_, _, _, MaxKey).


:- func tournament(psqueue(K, P), psqueue(K, P)) = psqueue(K, P).
tournament(PSQ1, PSQ2) = Res :-
    ( PSQ1 = void,
        Res = PSQ2
    ;
        PSQ1 = winner(K1, Prio1, L1, MaxKey1),
        ( PSQ2 = void,
            Res = PSQ1
        ;
            PSQ2 = winner(K2, Prio2, L2, MaxKey2),
            compare(CMP, Prio1, Prio2),
            ( CMP = (>) ->
                Res = winner(K2, Prio2, loser(K1, Prio1, L1, MaxKey2, L2), MaxKey2)
            ;
                Res = winner(K1, Prio1, loser(K2, Prio2, L1, MaxKey1, L2), MaxKey2)
            )
        )
    ).

:- func second_best(ltree(K, P), K) = psqueue(K, P) is det.
second_best(LTree, Key) = Res :-
    ( LTree = start,
        Res = void
    ;
        LTree = loser(LK, LP, L1, SplitKey, L2),
        compare(CMP, SplitKey, LK),
        ( CMP = (<) ->
            T1 = second_best(L1, SplitKey),
            T2 = winner(LK, LP, L2, Key),
            Res = tournament(T1, T2)
        ;
            T1 = winner(LK, LP, L1, SplitKey),
            T2 = second_best(L2, Key),
            Res = tournament(T1, T2)
        )
    ).

:- func del_min(psqueue(K, P)) = psqueue(K, P) is semidet.
del_min(PSQ) = Res :-
    PSQ = winner(_, _, L, MaxKey),
    Res = second_best(L, MaxKey).

:- pred loser_right(ltree(K, P)::in) is semidet.
loser_right(L) :-
    L = loser(LK, _, _, SplitKey, _),
    compare((>), LK, SplitKey).
