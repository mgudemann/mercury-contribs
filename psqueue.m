:- module psqueue.
:- interface.

:- type psqueue(K, P).

:- func init = psqueue(K, P).
:- pred init(psqueue(K, P)::out) is det.

:- pred is_empty(psqueue(K, P)::in) is semidet.

:- pred singleton(K::in, P::in, psqueue(K, P)::out) is det.
:- func singleton(K, P) = psqueue(K, P).

:- func max_key(psqueue(K, P)) = K is semidet.
:- pred max_key(psqueue(K, P)::in, K::out) is semidet.

:- func tournament(psqueue(K, P), psqueue(K, P)) = psqueue(K, P).
:- pred tournament(psqueue(K, P)::in, psqueue(K, P)::in, psqueue(K, P)::out) is det.

:- func del_min(psqueue(K, P)) = psqueue(K, P) is semidet.

:- pred leq(V::in, V::in) is semidet.

:- func min_view(psqueue(K, P)) = t_min_view(K, P) is det.
:- func tournament_view(psqueue(K, P)) = t_tournament_view(K, P) is det.

:- type t_min_view(K, P) --->
    emtpy
    ; min(K, P, psqueue(K, P)).

:- type t_tournament_view(K, P) --->
    emptySet
    ; singleton(K, P)
    ; tournament_between(psqueue(K, P), psqueue(K, P)).

:- implementation.

:- type psqueue(K, P) --->
    void
    ; winner(K, P, ltree(K, P), K).

:- type ltree_size == int.

:- type ltree(K, P) --->
    start
    ; lloser(ltree_size, K, P, ltree(K, P), K, ltree(K, P))
    ; rloser(ltree_size, K, P, ltree(K, P), K, ltree(K, P)).

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


tournament(PSQ0, PSQ1, PSQ) :-
    PSQ = tournament(PSQ0, PSQ1).

tournament(PSQ1, PSQ2) = Res :-
    ( PSQ1 = void,
        Res = PSQ2
    ;
        PSQ1 = winner(K1, Prio1, L1, MaxKey1),
        ( PSQ2 = void,
            Res = PSQ1
        ;
            PSQ2 = winner(K2, Prio2, L2, MaxKey2),
            Size = 0,
            ( Prio1 `leq` Prio2 ->
                % left wins
                Res = winner(K1, Prio1, rloser(Size, K2, Prio2, L1, MaxKey1, L2), MaxKey2)
            ;
                % right wins
                Res = winner(K2, Prio2, lloser(Size, K1, Prio1, L1, MaxKey1, L2), MaxKey2)
            )
        )
    ).

:- func second_best(ltree(K, P), K) = psqueue(K, P) is det.
second_best(LTree, Key) = Res :-
    ( LTree = start,
      Res = void
    ;
      ( LTree = rloser(_, LK, LP, L1, SplitKey, L2)
      ;
        LTree = lloser(_, LK, LP, L1, SplitKey, L2)),
      ( LK `leq` SplitKey ->
          T1 = winner(LK, LP, L1, SplitKey),
          T2 = second_best(L2, Key),
          Res = tournament(T1, T2)
      ;
          T1 = second_best(L1, SplitKey),
          T2 = winner(LK, LP, L2, Key),
          Res = tournament(T1, T2)
      )
    ).

del_min(PSQ) = Res :-
    PSQ = winner(_, _, L, MaxKey),
    Res = second_best(L, MaxKey).


% :- func lookup(K, psqueue(K, P)) = P is semidet.
% lookup(Key, PSQ) = Res :-
%     PSQ = winner(WKey, Prio, LTree, MaxKey),
%     ( LTree = start,
%         compare(CMP, Key, WKey),
%         CMP = (=),
%         Res = Prio
%     ;
%         ( LTree = lloser(_, _, _, LL, _, LR)
%         ;
%             LTree = rloser(_, _, _, LL, _, LR)),
%         compare(CMP, MaxKey, Key),
%         ( CMP = (<) ->
%             Res = lookup(Key, LR)
%         ;
%             Res = lookup(Key, LL)
%         )
%     ).

% less or equal
% is true if ValLeft =< ValRight
leq(ValLeft, ValRight) :-
    compare(CMP, ValLeft, ValRight),
    ( CMP = (>) -> fail; true).

min_view(PSQ) = Res :-
    PSQ = void, Res = emtpy
    ;
    PSQ = winner(Key, Prio, LTree, MaxKey),
    Res = min(Key, Prio, second_best(LTree, MaxKey)).

tournament_view(PSQ) = Res :-
    PSQ = void, Res = emptySet
    ;
    PSQ = winner(K, P, LTree, MaxKey),
    (
      LTree = start, Res = singleton(K, P)
    ;
      ( LTree = rloser(_, LK, LP, LL, SplitKey, LR)
      ;
          LTree = lloser(_, LK, LP, LL, SplitKey, LR)
      ),
      ( LK `leq` SplitKey ->
          Res = tournament_between(winner(LK, LP, LL, SplitKey),
                                   winner(K, P, LR, MaxKey))
      ;
          Res = tournament_between(winner(K, P, LL, SplitKey),
                                   winner(LK, LP, LR, MaxKey))
      )
    ).

:- func lookup(K, psqueue(K, P)) = P is semidet.
lookup(K, PSQ) = lookup_tv(K, tournament_view(PSQ)).

:- func lookup_tv(K, t_tournament_view(K, P)) = P is semidet.
lookup_tv(K, TV) = Res :-
    TV = singleton(Key, Prio),
    Key = K,
    Res = Prio
    ;
    TV = tournament_between(W1, W2),
    W1 = winner(_, _, _, MaxKey1),
    ( K `leq` MaxKey1 ->
        Res = lookup(K, W1)
    ;
        Res = lookup(K, W2)
    ).


:- func adjust(func(P) = P, K, psqueue(K, P)) = psqueue(K, P) is semidet.
adjust(F, K, PSQ) = adjust_tv(F, K, tournament_view(PSQ)).

:- func adjust_tv(func(P) = P, K, t_tournament_view(K, P)) = psqueue(K, P) is semidet.
adjust_tv(Func, K, TV) = Res :-
    TV = emptySet, Res = void
    ;
    TV = singleton(Key, Prio),
    ( K = Key ->
        Res = psqueue.singleton(Key, Func(Prio))
    ;
        Res = psqueue.singleton(Key, Prio)
    )
    ;
    TV = tournament_between(T1, T2),
    T1 = winner(_, _, _, MaxKey1),
    ( K `leq` MaxKey1 ->
        Res = tournament(adjust(Func, K, T1), T2)
    ;
        Res = tournament(T1, adjust(Func, K, T2))
    ).

:- func insert(K, P, psqueue(K, P)) = psqueue(K, P) is semidet.
insert(IK, IP, PSQ) = insert_tv(IK, IP, tournament_view(PSQ)).

:- func insert_tv(K, P, t_tournament_view(K, P)) = psqueue(K, P) is semidet.
insert_tv(IK, IP, TV) = Res :-
    TV = emptySet, Res = void
    ;
    TV = singleton(Key, Prio),
    compare(CMP, IK, Key),
    ( CMP = (<),
        Res = tournament(psqueue.singleton(Key, Prio),
                         psqueue.singleton(IK, IP))
    ;
        CMP = (=),
        Res = psqueue.singleton(IK, IP)
    ;
        CMP = (>),
        Res = tournament(psqueue.singleton(IK, IP),
                         psqueue.singleton(Key, Prio))
    )
    ;
    TV = tournament_between(T1, T2),
    T1 = winner(_, _, _, MaxKey1),
    T2 = winner(_, _, _, _),
    ( IK `leq` MaxKey1 ->
        Res = tournament(insert(IK, IP, T1), T2)
    ;
        Res = tournament(T1, insert(IK, IP, T2))
    ).

:- func delete(K, psqueue(K, P)) = psqueue(K, P) is semidet.
delete(DK, PSQ) = delete_tv(DK, tournament_view(PSQ)).

:- func delete_tv(K, t_tournament_view(K, P)) = psqueue(K, P) is semidet.
delete_tv(DK, TV) = Res :-
    TV = emptySet, Res = void
    ;
    TV = singleton(Key, Prio),
    ( DK = Key ->
        Res = void
    ;
        Res = psqueue.singleton(Key, Prio)
    )
    ;
    TV = tournament_between(T1, T2),
    T1 = winner(_, _, _, MaxKey1),
    ( DK `leq` MaxKey1 ->
        Res = tournament(delete(DK, T1), T2)
    ;
        Res = tournament(T1, delete(DK, T2))
    ).
