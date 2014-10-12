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

:- implementation.

:- type psqueue(K, P) --->
    void
    ; winner(K, P, ltree(K, P), K).

:- type ltree(K, P) --->
    start
    ; lloser(int, K, P, ltree(K, P), K, ltree(K, P))
    ; rloser(int, K, P, ltree(K, P), K, ltree(K, P)).

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
:- pred leq(V::in, V::in) is semidet.
leq(ValLeft, ValRight) :-
    compare(CMP, ValLeft, ValRight),
    ( CMP = (>) -> fail; true).