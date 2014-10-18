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

:- func tournament(psqueue(K, P), psqueue(K, P)) = psqueue(K, P) is det.
:- pred tournament(psqueue(K, P)::in, psqueue(K, P)::in, psqueue(K, P)::out) is det.

:- pred del_min(psqueue(K, P)::in, K::out, P::out, psqueue(K, P)::out) is semidet.

:- func delete(K, psqueue(K, P)) = psqueue(K, P) is semidet.
:- func insert(K, P, psqueue(K, P)) = psqueue(K, P) is semidet.
:- func det_insert(K, P, psqueue(K, P)) = psqueue(K, P) is det.
:- func adjust(func(P) = P, K, psqueue(K, P)) = psqueue(K, P) is semidet.
:- func lookup(K, psqueue(K, P)) = P is semidet.


:- pred leq(V::in, V::in) is semidet.

:- func min_view(psqueue(K, P)) = t_min_view(K, P) is det.
:- func tournament_view(psqueue(K, P)) = t_tournament_view(K, P) is det.
:- func tree_view(ltree(K, P)) = t_tree_view(K, P) is det.

:- type t_min_view(K, P) --->
    emtpy
    ; min(K, P, psqueue(K, P)).

:- type t_tournament_view(K, P) --->
    emptySet
    ; singleton(K, P)
    ; tournament_between(psqueue(K, P), psqueue(K, P)).

:- type t_tree_view(K, P) --->
    leaf
    ; node(K, P, ltree(K, P), K, ltree(K, P)).

:- implementation.

:- import_module int.
:- import_module require.

:- type psqueue(K, P) --->
    void
    ;
    winner(K, P, ltree(K, P), K).

:- type t_ltree_size == int.

:- type ltree(K, P) --->
    start
    ;
    loser(t_ltree_size, K, P, ltree(K, P), K, ltree(K, P)).


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
            ( Prio1 `leq` Prio2 ->
                % left wins
                Res = winner(K1, Prio1,
                             balance(K2, Prio2, L1, MaxKey1, L2), MaxKey2)
            ;
                % right wins
                Res = winner(K2, Prio2,
                             balance(K1, Prio1, L1, MaxKey1, L2), MaxKey2)
            )
        )
    ).

:- func second_best(ltree(K, P), K) = psqueue(K, P) is det.
second_best(LTree, Key) = Res :-
    ( LTree = start,
      Res = void
    ;
      LTree = loser(_, LK, LP, L1, SplitKey, L2),
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

del_min(PSQ, MinKey, MinPrio, NewPSQ) :-
    PSQ = winner(MinKey, MinPrio, L, MaxKey),
    NewPSQ = second_best(L, MaxKey).

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
      LTree = loser(_, LK, LP, LL, SplitKey, LR),
      ( LK `leq` SplitKey ->
          Res = tournament_between(winner(LK, LP, LL, SplitKey),
                                   winner(K, P, LR, MaxKey))
      ;
          Res = tournament_between(winner(K, P, LL, SplitKey),
                                   winner(LK, LP, LR, MaxKey))
      )
    ).

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

insert(IK, IP, PSQ) = insert_tv(IK, IP, tournament_view(PSQ)).

det_insert(IK, IP, PSQ) = Res :-
        ( Res0 = insert_tv(IK, IP, tournament_view(PSQ)) ->
          Res = Res0
        ;
          unexpected($file, $pred, "error in deterministic insert")
        ).

:- func insert_tv(K, P, t_tournament_view(K, P)) = psqueue(K, P) is semidet.
insert_tv(IK, IP, TV) = Res :-
    TV = emptySet, Res = psqueue.singleton(IK, IP)
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

tree_view(LTree) = Res :-
    LTree = start, Res = leaf
    ;
    LTree = loser(_, LK, LP, LL, SplitKey, LR),
    Res = node(LK, LP, LL, SplitKey, LR).

:- func ltree_size(ltree(K, P)) = t_ltree_size is det.
ltree_size(LTree) = Res :-
    LTree = start, Res = 0
    ;
    LTree = loser(Res, _, _, _, _, _).

% smart constructors

:- func construct_leaf = ltree(K, P).
construct_leaf = start.

:- func construct_node(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P).
construct_node(Key, Prio, L, SplitKey, R) = Res :-
    Size = 1 + ltree_size(L) + ltree_size(R),
    Res = loser(Size, Key, Prio, L, SplitKey, R).

:- func balance_omega = t_ltree_size.
:- func balance(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func balance_left(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func balance_right(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func single_left(K, P, ltree(K, P), K, t_tree_view(K, P)) = ltree(K, P) is det.
:- func single_right(K, P, t_tree_view(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func double_left(K, P, ltree(K, P), K, t_tree_view(K, P)) = ltree(K, P) is det.
:- func double_right(K, P, t_tree_view(K, P), K, ltree(K, P)) = ltree(K, P) is det.

balance_omega = 4.

balance(Key, Prio, L, SplitKey, R) = Res :-
    SizeL = ltree_size(L),
    SizeR = ltree_size(R),
    ( (SizeR + SizeL) `leq` 2 ->
        Res = construct_node(Key, Prio, L, SplitKey, R)
    ;
        (( compare(CMP, SizeR, balance_omega * SizeL), CMP = (>)) ->
            Res = balance_left(Key, Prio, L, SplitKey, R)
        ;
            (( compare(CMP, SizeL, balance_omega * SizeR), CMP = (>)) ->
                Res = balance_right(Key, Prio, L, SplitKey, R)
            ;
                Res = construct_node(Key, Prio, L, SplitKey, R)
            )
        )
    ).

balance_left(Key, Prio, L, SplitKey, R) = Res :-
    TVR = tree_view(R),
    ( TVR = node(_, _, RL, _, RR) ->
        ( (compare(CMP, ltree_size(RL), ltree_size(RR)), CMP = (<)) ->
            Res = single_left(Key, Prio, L, SplitKey, TVR)
        ;
            Res = double_left(Key, Prio, L, SplitKey, TVR)
        )
    ;
        unexpected($file, $pred, "error in left balance")
    ).

balance_right(Key, Prio, L, SplitKey, R) = Res :-
    TVL = tree_view(L),
    ( TVL = node(_, _, LL, _, LR) ->
        ( (compare(CMP, ltree_size(LR), ltree_size(LL)), CMP = (<)) ->
            Res = single_right(Key, Prio, TVL, SplitKey, R)
        ;
            Res = double_right(Key, Prio, TVL, SplitKey, R)
        )
    ;
        unexpected($file, $pred, "error in right balance")
    ).

single_left(K1, P1, T1, S1, TVR) = Res :-
    ( TVR = node(K2, P2, T2, S2, T3) ->
        ( ( K2 `leq` S2, P1 `leq` P2 ) ->
            Res = construct_node(K1, P1, construct_node(K2, P2, T1, S1, T2), S2, T3)
        ;
            Res = construct_node(K2, P2, construct_node(K1, P1, T1, S1, T2), S2, T3)
        )
    ;
        unexpected($file, $pred, "error in single left rotation")
    ).

single_right(K1, P1, TVL, S2, T3) = Res :-
    ( TVL = node(K2, P2, T1, S1, T2) ->
        ( ( compare(CMP0, K2, S1), CMP0 = (>), P1 `leq` P2 ) ->
            Res = construct_node(K1, P1, T1, S1, construct_node(K2, P2, T2, S2, T3))
        ;
            Res = construct_node(K2, P2, T1, S1, construct_node(K1, P1, T2, S1, T3))
        )
    ;
        unexpected($file, $pred, "error in single right rotation")
    ).

double_left(K1, P1, T1, S1, TVR) = Res :-
    ( TVR = node(K2, P2, T2, S2, T3) ->
        Res = single_left(K1, P1, T1, S1,
                          tree_view(single_right(K2, P2,
                                                 tree_view(T2), S2, T3)))
    ;
        unexpected($file, $pred, "error in doulbe left rotation")
    ).

double_right(K1, P1, TVL, S2, T3) = Res :-
    ( TVL = node(K2, P2, T1, S1, T2) ->
        Res = single_right(K1, P1,
                           tree_view(single_left(K2, P2, T1, S1,
                                                 tree_view(T2))),
                           S2, T3)
    ;
        unexpected($file, $pred, "error in double right rotation")
    ).
