%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% File: psqueue.m.
% Main author: guedemann.
% Stability: low.
%
% This module implements a priority search queue ADT.
%
% A psqueue is a priority search queue. A priority search queue holds a
% collection of key-value pairs together with a priority; the interface
% provides operations to create an empty priority queue, to insert a key-value
% pair with a priority into a priority queue, to remove the element with the
% lowest key, to look up a key-value pair and its priority and to adjust the
% priority of a key-value pair.
%
% The implementation here follows closely the description given in Ralf Hinze's
% paper "A Simple Implementation Technique for Priority Search Queues", ICFP
% 2001, pp. 110-121. The main difference is that psqueue have an additional
% value field instead of only a key-priority pair.
%
% The key-value-priority triplets are stored in a weight-balanced tree to
% guarantee efficient access and updates.
%
% peeking at the highest priority element O(1)
% removing the highest priority element   O(log n)
% adjust / lookup / deletion / insert     O(log n)
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module psqueue.
:- interface.

%---------------------------------------------------------------------------%

:- type psqueue(K, P, V).

    % create an empty priority search queue
    %
:- func init = psqueue(K, P, V).
:- pred init(psqueue(K, P, V)::out) is det.

    % true iff the priority search queue is empty.
    %
:- pred is_empty(psqueue(K, P, V)::in) is semidet.

    % remove element with minimal priority
    %
:- pred del_min(psqueue(K, P, V)::in, K::out, P::out, V::out,
               psqueue(K, P, V)::out) is semidet.

    % remove element with specific key from priority queue
    %
:- func delete(K, psqueue(K, P, V)) = psqueue(K, P, V) is semidet.

    % insert key with specified priority into priority search queue
    %
:- func insert(K, P, V, psqueue(K, P, V)) = psqueue(K, P, V)
    is semidet.

    % As above, will call error/1 if the key is already present
    %
:- func det_insert(K, P, V, psqueue(K, P, V)) = psqueue(K, P, V) is det.

    % adjust priority of specified element. The old priority is given as an
    % argument to the adjustment function
    %
:- func adjust(func(P) = P, K, psqueue(K, P, V)) = psqueue(K, P, V) is semidet.

    % lookup the priority of the specified element
    %
:- pred lookup(K::in, psqueue(K, P, V)::in, P::out, V::out) is semidet.

    % return the size of the priority search queue as the number of elements
    %
:- func size(psqueue(K, P, V)) = int is det.
:- pred size(psqueue(K, P, V)::in, int::out) is det.

    % true if the priority search queue respects the semi heap properties,
    % i.e., 1) the top element has the highest priority and 2) for each node of
    % the loser tree, the priority of the loser is higher or equal to the
    % priorities in the subtree from which the loser originates
    %
:- pred is_semi_heap(psqueue(K, P, V)::in) is semidet.

    % true if the priority search queue respects the search tree properties,
    % i.e., for each node the keys in the left subtree are smaller as or equal
    % to the split key and the keys in the right subtree are larger than the
    % split key
    %
:- pred is_search_tree(psqueue(K, P, V)::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- type psqueue(K, P, V) --->
      void
    ; winner(K, P, V, ltree(K, P, V), K).

:- type t_ltree_size == int.

:- type ltree(K, P, V) --->
      start
    ; loser(t_ltree_size, K, P, V, ltree(K, P, V), K, ltree(K, P, V)).

%---------------------------------------------------------------------------%

% create empty psqueue
psqueue.init = PSQ :-
        psqueue.init(PSQ).

psqueue.init(void).

% check for empty psqueue
psqueue.is_empty(void).


    % create singleton psqueue
    %
:- pred singleton(K::in, P::in, V::in, psqueue(K, P, V)::out) is det.
:- func singleton(K, P, V) = psqueue(K, P, V).

singleton(K, P, V) = Res :-
    singleton(K, P, V, Res).

singleton(K, P, V, PSQ) :-
    PSQ = winner(K, P, V, start, K).


    % extract maximal (highest priority) key
    %
:- func max_key(psqueue(K, P, V)) = K is semidet.
:- pred max_key(psqueue(K, P, V)::in, K::out) is semidet.

max_key(PSQ) = K :-
        max_key(PSQ, K).

max_key(PSQ, MaxKey) :-
        PSQ = winner(_, _, _, _, MaxKey).


    % play tournament to combine two priority search queues, see Ralf Hinze's
    % paper for explanantion
    %
:- func tournament(psqueue(K, P, V), psqueue(K, P, V)) = psqueue(K, P, V)
    is det.
:- pred tournament(psqueue(K, P, V)::in, psqueue(K, P, V)::in,
                  psqueue(K, P, V)::out) is det.

    % generate specialized code for integral priorities
    %
:- pragma type_spec(tournament/3, P = int).

tournament(PSQ0, PSQ1, PSQ) :-
    PSQ = tournament(PSQ0, PSQ1).

tournament(PSQ1, PSQ2) = Res :-
    ( PSQ1 = void,
        Res = PSQ2
    ;
        PSQ1 = winner(K1, Prio1, Val1, L1, MaxKey1),
        ( PSQ2 = void,
            Res = PSQ1
        ;
            PSQ2 = winner(K2, Prio2, Val2, L2, MaxKey2),
            ( Prio1 `leq` Prio2 ->
                % left wins
                Res = winner(K1, Prio1, Val1,
                             balance(K2, Prio2, Val2, L1, MaxKey1, L2),
                             MaxKey2)
            ;
                % right wins
                Res = winner(K2, Prio2, Val2,
                             balance(K1, Prio1, Val1, L1, MaxKey1, L2),
                             MaxKey2)
            )
        )
    ).


:- func second_best(ltree(K, P, V), K) = psqueue(K, P, V) is det.
second_best(LTree, Key) = Res :-
    (
      LTree = start,
      Res = void
    ;
      LTree = loser(_, LK, LP, LV, T, SplitKey, U),
      ( LK `leq` SplitKey ->
          T1 = winner(LK, LP, LV, T, SplitKey),
          T2 = second_best(U, Key),
          Res = tournament(T1, T2)
      ;
          T1 = second_best(T, SplitKey),
          T2 = winner(LK, LP, LV, U, Key),
          Res = tournament(T1, T2)
      )
    ).

del_min(PSQ, MinKey, MinPrio, MinVal, NewPSQ) :-
    PSQ = winner(MinKey, MinPrio, MinVal, L, MaxKey),
    NewPSQ = second_best(L, MaxKey).


:- pred leq(V::in, V::in) is semidet.
:- pragma type_spec(leq/2, V = int).
leq(ValLeft, ValRight) :-
    compare(CMP, ValLeft, ValRight),
    ( CMP = (>) ->
        fail
    ;
        true).

%---------------------------------------------------------------------------%
% view types for min view, tournament view and tree view
%---------------------------------------------------------------------------%

:- type t_min_view(K, P, V) --->
      emtpy
    ; min(K, P, V, psqueue(K, P, V)).

:- type t_tournament_view(K, P, V) --->
      emptySet
    ; singleton(K, P, V)
    ; tournament_between(psqueue(K, P, V), psqueue(K, P, V)).

:- type t_tree_view(K, P, V) --->
      leaf
    ; node(K, P, V, ltree(K, P, V), K, ltree(K, P, V)).

%---------------------------------------------------------------------------%

    % get min view of priority search queue
    %
:- func min_view(psqueue(K, P, V)) = t_min_view(K, P, V) is det.

min_view(PSQ) = Res :-
    (
      PSQ = void, Res = emtpy
    ;
      PSQ = winner(Key, Prio, Val, LTree, MaxKey),
      Res = min(Key, Prio, Val, second_best(LTree, MaxKey))
    ).

    % get tournament view of priority search queue
    %
:- func tournament_view(psqueue(K, P, V)) = t_tournament_view(K, P, V) is det.

tournament_view(PSQ) = Res :-
    PSQ = void, Res = emptySet
    ;
    PSQ = winner(K, P, V, LTree, MaxKey),
    (
      LTree = start, Res = singleton(K, P, V)
    ;
      LTree = loser(_, LK, LP, LV, TL, SplitKey, TR),
      ( LK `leq` SplitKey ->
          Res = tournament_between(winner(LK, LP, LV, TL, SplitKey),
                                   winner(K, P, V, TR, MaxKey))
      ;
          Res = tournament_between(winner(K, P, V, TL, SplitKey),
                                   winner(LK, LP, LV, TR, MaxKey))
      )
    ).


    % get tree view of priority search queue
    %
:- func tree_view(ltree(K, P, V)) = t_tree_view(K, P, V) is det.
tree_view(LTree) = Res :-
    (
      LTree = start, Res = leaf
    ;
      LTree = loser(_, LK, LP, LV, LL, SplitKey, LR),
      Res = node(LK, LP, LV, LL, SplitKey, LR)
    ).

lookup(K, PSQ, P, V) :-
    lookup_tv(K, tournament_view(PSQ), P, V).

:- pred lookup_tv(K::in, t_tournament_view(K, P, V)::in, P::out, V::out)
    is semidet.
lookup_tv(K, TV, P, V) :-
    (
      TV = singleton(Key, Prio, Val),
      Key = K,
      P = Prio,
      V = Val
    ;
      TV = tournament_between(TL, TR),
      TL = winner(_, _, _, _, MaxKey1),
      ( K `leq` MaxKey1 ->
          lookup(K, TL, P, V)
      ;
          lookup(K, TR, P, V)
      )
    ).


adjust(F, K, PSQ) = adjust_tv(F, K, tournament_view(PSQ)).

:- func adjust_tv(func(P) = P, K, t_tournament_view(K, P, V)) =
    psqueue(K, P, V) is semidet.
adjust_tv(Func, K, TV) = Res :-
    (
      TV = emptySet, Res = void
    ;
      TV = singleton(Key, Prio, Val),
      ( K = Key ->
          Res = psqueue.singleton(Key, Func(Prio), Val)
      ;
          Res = psqueue.singleton(Key, Prio, Val)
      )
    ;
      TV = tournament_between(TL, TR),
      TL = winner(_, _, _, _, MaxKey1),
      ( K `leq` MaxKey1 ->
          Res = tournament(adjust(Func, K, TL), TR)
      ;
          Res = tournament(TL, adjust(Func, K, TR))
      )
    ).

insert(IK, IP, IV, PSQ) = insert_tv(IK, IP, IV, tournament_view(PSQ)).

det_insert(IK, IP, IV, PSQ) = Res :-
        ( Res0 = insert_tv(IK, IP, IV, tournament_view(PSQ)) ->
            Res = Res0
        ;
            unexpected($file, $pred, "error in deterministic insert")
        ).

:- func insert_tv(K, P, V, t_tournament_view(K, P, V)) = psqueue(K, P, V)
    is semidet.
insert_tv(IK, IP, IV, TV) = Res :-
    (
      TV = emptySet, Res = psqueue.singleton(IK, IP, IV)
    ;
      TV = singleton(Key, Prio, Val),
      compare(CMP, IK, Key),
      (
        CMP = (<), Res = tournament(psqueue.singleton(IK, IP, IV),
                                    psqueue.singleton(Key, Prio, Val))
      ;
        CMP = (=), Res = psqueue.singleton(IK, IP, IV)
      ;
        CMP = (>), Res = tournament(psqueue.singleton(Key, Prio, Val),
                                    psqueue.singleton(IK, IP, IV))
      )
    ;
      TV = tournament_between(T1, T2),
      T1 = winner(_, _, _, _, MaxKey1),
      T2 = winner(_, _, _, _, _),
      ( IK `leq` MaxKey1 ->
          Res = tournament(insert(IK, IP, IV, T1), T2)
      ;
          Res = tournament(T1, insert(IK, IP, IV, T2))
      )
    ).

delete(DK, PSQ) = delete_tv(DK, tournament_view(PSQ)).

:- func delete_tv(K, t_tournament_view(K, P, V)) = psqueue(K, P, V) is semidet.
delete_tv(DK, TV) = Res :-
    (
      (
        TV = emptySet, Res = void
      ;
        TV = singleton(Key, Prio, Val),
        ( DK = Key ->
            Res = void
        ;
            Res = psqueue.singleton(Key, Prio, Val)
        )
      )
    ;
      TV = tournament_between(TL, TR),
      TL = winner(_, _, _, _, MaxKey1),
      ( DK `leq` MaxKey1 ->
          Res = tournament(delete(DK, TL), TR)
      ;
          Res = tournament(TL, delete(DK, TR))
      )
    ).

size(PSQ, Size) :-
    (
      PSQ = void, Size = 0
    ;
      PSQ = winner(_, _, _, LTree, _),
      Size = ltree_size(LTree)
    ).

size(PSQ) = Res :-
    size(PSQ, Res).

:- func ltree_size(ltree(K, P, V)) = t_ltree_size is det.
ltree_size(LTree) = Res :-
    (
      LTree = start, Res = 0
    ;
      LTree = loser(Res, _, _, _, _, _, _)
    ).

%---------------------------------------------------------------------------%
% smart constructors
%---------------------------------------------------------------------------%

:- func construct_leaf = ltree(K, P, V).
construct_leaf = start.

:- func construct_node(K, P, V, ltree(K, P, V), K, ltree(K, P, V)) =
    ltree(K, P, V).
construct_node(Key, Prio, Val, L, SplitKey, R) = Res :-
    Size = 1 + ltree_size(L) + ltree_size(R),
    Res = loser(Size, Key, Prio, Val, L, SplitKey, R).


%---------------------------------------------------------------------------%
% balancing functions for weight-balanced tree
%---------------------------------------------------------------------------%


    % balance factor, must be over 3.75 (see Ralf Hinze's paper)
    %
:- func balance_omega = t_ltree_size.
balance_omega = 4.

:- func balance(K, P, V, ltree(K, P, V), K, ltree(K, P, V)) = ltree(K, P, V)
    is det.
:- func balance_left(K, P, V, ltree(K, P, V), K, ltree(K, P, V)) =
    ltree(K, P, V) is det.
:- func balance_right(K, P, V, ltree(K, P, V), K, ltree(K, P, V)) =
    ltree(K, P, V) is det.
:- func single_left(K, P, V, ltree(K, P, V), K, t_tree_view(K, P, V)) =
    ltree(K, P, V) is det.
:- func single_right(K, P, V, t_tree_view(K, P, V), K, ltree(K, P, V)) =
    ltree(K, P, V) is det.
:- func double_left(K, P, V, ltree(K, P, V), K, t_tree_view(K, P, V)) =
    ltree(K, P, V) is det.
:- func double_right(K, P, V, t_tree_view(K, P, V), K, ltree(K, P, V)) =
    ltree(K, P, V) is det.

balance(Key, Prio, Val, L, SplitKey, R) = Res :-
    SizeL = ltree_size(L),
    SizeR = ltree_size(R),
    ( (SizeR + SizeL) < 2 ->
        Res = construct_node(Key, Prio, Val, L, SplitKey, R)
    ;
        (( compare(CMP, SizeR, balance_omega * SizeL), CMP = (>)) ->
            Res = balance_left(Key, Prio, Val, L, SplitKey, R)
        ;
            (( compare(CMP, SizeL, balance_omega * SizeR), CMP = (>)) ->
                Res = balance_right(Key, Prio, Val, L, SplitKey, R)
            ;
                Res = construct_node(Key, Prio, Val, L, SplitKey, R)
            )
        )
    ).

balance_left(Key, Prio, Val, L, SplitKey, R) = Res :-
    TVR = tree_view(R),
    ( TVR = node(_, _, _, RL, _, RR) ->
        ( (compare(CMP, ltree_size(RL), ltree_size(RR)), CMP = (<)) ->
            Res = single_left(Key, Prio, Val, L, SplitKey, TVR)
        ;
            Res = double_left(Key, Prio, Val, L, SplitKey, TVR)
        )
    ;
        unexpected($file, $pred, "error in left balance")
    ).

balance_right(Key, Prio, Val, L, SplitKey, R) = Res :-
    TVL = tree_view(L),
    ( TVL = node(_, _, _, LL, _, LR) ->
        ( (compare(CMP, ltree_size(LR), ltree_size(LL)), CMP = (<)) ->
            Res = single_right(Key, Prio, Val, TVL, SplitKey, R)
        ;
            Res = double_right(Key, Prio, Val, TVL, SplitKey, R)
        )
    ;
        unexpected($file, $pred, "error in right balance")
    ).

single_left(K1, P1, V1, T1, S1, TVR) = Res :-
    ( TVR = node(K2, P2, V2, T2, S2, T3) ->
        ( ( K2 `leq` S2, P1 `leq` P2 ) ->
            Res = construct_node(K1, P1, V1,
                                 construct_node(K2, P2, V2, T1, S1, T2),
                                 S2, T3)
        ;
            Res = construct_node(K2, P2, V2,
                                 construct_node(K1, P1, V1, T1, S1, T2),
                                 S2, T3)
        )
    ;
        unexpected($file, $pred, "error in single left rotation")
    ).

single_right(K1, P1, V1, TVL, S2, T3) = Res :-
    ( TVL = node(K2, P2, V2, T1, S1, T2) ->
        ( ( compare(CMP0, K2, S1), CMP0 = (>), P1 `leq` P2 ) ->
            Res = construct_node(K1, P1, V1, T1, S1,
                                 construct_node(K2, P2, V2, T2, S2, T3))
        ;
            Res = construct_node(K2, P2, V2, T1, S1,
                                 construct_node(K1, P1, V1, T2, S2, T3))
        )
    ;
        unexpected($file, $pred, "error in single right rotation")
    ).

double_left(K1, P1, V1, T1, S1, TVR) = Res :-
    ( TVR = node(K2, P2, V2, T2, S2, T3) ->
        Res = single_left(K1, P1, V1, T1, S1,
                          tree_view(single_right(K2, P2, V2,
                                                 tree_view(T2), S2, T3)))
    ;
        unexpected($file, $pred, "error in doulbe left rotation")
    ).

double_right(K1, P1, V1, TVL, S2, T3) = Res :-
    ( TVL = node(K2, P2, V2, T1, S1, T2) ->
        Res = single_right(K1, P1, V1,
                           tree_view(single_left(K2, P2, V2, T1, S1,
                                                 tree_view(T2))),
                           S2, T3)
    ;
        unexpected($file, $pred, "error in double right rotation")
    ).

%---------------------------------------------------------------------------%
% test predicates for correct implementation of psqueue
%---------------------------------------------------------------------------%

is_semi_heap(PSQ) :-
    (
      PSQ = void
    ;
      PSQ = winner(_, Prio, _, LTree, _),
      all_keys_larger_ltree(Prio, LTree),
      all_nodes_loser_prio(LTree)
    ).

:- pred all_keys_larger_ltree(P::in, ltree(K, P, V)::in) is semidet.
all_keys_larger_ltree(Prio, LTree) :-
    LTree = start
    ;
    LTree = loser(_, _, LP, _, LT, _, RT),
    Prio `leq` LP,
    all_keys_larger_ltree(Prio, LT),
    all_keys_larger_ltree(Prio, RT).

:- pred min_prio_loser_tree(ltree(K, P, V)::in, P::out) is semidet.
:- pred min_prio_loser_tree(ltree(K, P, V)::in, P::in, P::out) is det.

min_prio_loser_tree(LTree, MinPrio) :-
    LTree = loser(_, _, Prio, _, TL, _, TR),
    min_prio_loser_tree(TL, Prio, MinPrio1),
    min_prio_loser_tree(TR, Prio, MinPrio2),
    ( MinPrio1 `leq` MinPrio2 ->
        MinPrio = MinPrio1; MinPrio = MinPrio2).

min_prio_loser_tree(LTree, CurrMin, MinPrio) :-
    LTree = start, MinPrio = CurrMin
    ;
    LTree = loser(_, _,  Prio, _, TL, _, TR),
    ( CurrMin `leq` Prio ->
        min_prio_loser_tree(TL, CurrMin, MinPrio1),
        min_prio_loser_tree(TR, CurrMin, MinPrio2)
    ;
        min_prio_loser_tree(TL, Prio, MinPrio1),
        min_prio_loser_tree(TR, Prio, MinPrio2)
    ),
    ( MinPrio1 `leq` MinPrio2 ->
        MinPrio = MinPrio1; MinPrio = MinPrio2).

:- pred all_nodes_loser_prio(ltree(K, P, V)::in) is semidet.
all_nodes_loser_prio(LTree) :-
    LTree = start
    ;
    LTree = loser(_, K, Prio, _, TL, SplitKey, TR),
    ( K `leq` SplitKey ->
        min_prio_loser_tree(TL, Prio, MinPrio)
    ;
        min_prio_loser_tree(TR, Prio, MinPrio)
    ),
    compare(CMP, Prio, MinPrio),
    CMP = (=),
    all_nodes_loser_prio(TL),
    all_nodes_loser_prio(TR).

is_search_tree(PSQ) :-
    (
      PSQ = void
    ;
      PSQ = winner(_, _, _, LTree, _),
      all_search_keys(LTree)
    ).

:- pred all_search_keys(ltree(K, P, V)::in) is semidet.
all_search_keys(LTree) :-
    (
      LTree = start
    ;
      LTree = loser(_, K, _, _, TL, _, TR),
      max_key_loser_tree(TL, MaxKeyL),
      min_key_loser_tree(TR, MinKeyR),
      MaxKeyL `leq` K,
      K `leq` MinKeyR,
      all_search_keys(TL),
      all_search_keys(TR)
    ).


:- pred min_key_loser_tree(ltree(K, P, V)::in, K::out) is semidet.
:- pred min_key_loser_tree(ltree(K, P, V)::in, K::in, K::out) is det.

min_key_loser_tree(LTree, MinKey) :-
    LTree = loser(_, Key, _, _, TL, _, TR),
    min_key_loser_tree(TL, Key, MinKey1),
    min_key_loser_tree(TR, Key, MinKey2),
    ( MinKey1 `leq` MinKey2 ->
        MinKey = MinKey1; MinKey = MinKey2).

min_key_loser_tree(LTree, CurrMin, MinKey) :-
    LTree = start, MinKey = CurrMin
    ;
    LTree = loser(_, Key, _, _, TL, _, TR),
    ( CurrMin `leq` Key ->
        min_key_loser_tree(TL, CurrMin, MinKey1),
        min_key_loser_tree(TR, CurrMin, MinKey2)
    ;
        min_key_loser_tree(TL, Key, MinKey1),
        min_key_loser_tree(TR, Key, MinKey2)
    ),
    ( MinKey1 `leq` MinKey2 ->
        MinKey = MinKey1; MinKey = MinKey2).

:- pred max_key_loser_tree(ltree(K, P, V)::in, K::out) is semidet.
:- pred max_key_loser_tree(ltree(K, P, V)::in, K::in, K::out) is det.

max_key_loser_tree(LTree, MaxKey) :-
    LTree = loser(_, Key, _, _, TL, _, TR),
    min_key_loser_tree(TL, Key, MaxKey1),
    min_key_loser_tree(TR, Key, MaxKey2),
    ( MaxKey2 `leq` MaxKey1 ->
        MaxKey = MaxKey2; MaxKey = MaxKey1).

max_key_loser_tree(LTree, CurrMax, MaxKey) :-
    LTree = start, MaxKey = CurrMax
    ;
    LTree = loser(_, Key, _, _, TL, _, TR),
    ( CurrMax `leq` Key ->
        min_key_loser_tree(TL, CurrMax, MaxKey1),
        min_key_loser_tree(TR, CurrMax, MaxKey2)
    ;
        min_key_loser_tree(TL, Key, MaxKey1),
        min_key_loser_tree(TR, Key, MaxKey2)
    ),
    ( MaxKey2 `leq` MaxKey1 ->
        MaxKey = MaxKey2; MaxKey = MaxKey1).

%---------------------------------------------------------------------------%

:- end_module psqueue.