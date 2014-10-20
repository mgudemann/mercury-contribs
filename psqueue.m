%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% File: psqueue.m.
% Main author: mgudemann.
% Stability: low.
%
% This module implements a priority search queue ADT.
%
% A psqueue is a priority search queue. A priority search queue holds a
% collection of keys together with a priority; the interface provides
% operations to create an empty priority queue, to insert a key with an
% associated priority into a priority queue, to remove the element with the
% highest priority, to look up a key and its priority and to adjust the
% priority of a key.
%
% The implementation here follows closely the description given in Ralf Hinze's
% paper "A Simple Implementation Technique for Priority Search Queues", ICFP
% 2001, pp. 110-121.
%
% The key-priority pairs are stored in a weight-balanced tree for efficient
% acces.
%
% read highest priority element:       O(1)
% remove highest priority element      O(log n)
% delete/insert/ajdust/lookup element: O(log n)
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module psqueue.
:- interface.

:- import_module assoc_list.

%---------------------------------------------------------------------------%

:- type psqueue(K, P).

    % create an empty priority search queue
    %
:- func init = psqueue(K, P).
:- pred init(psqueue(K, P)::out) is det.

    % true iff the priority search queue is empty.
    %
:- pred is_empty(psqueue(K, P)::in) is semidet.

    % remove element with minimal priority
    %
:- pred del_min(psqueue(K, P)::in, K::out, P::out, psqueue(K, P)::out)
    is semidet.

    % peek at highest priority key
    %
:- pred peek(psqueue(K, P)::in, K::out, P::out) is semidet.

    % as peek/3, will call error/1 if the psqueue is empty
    %
:- pred det_peek(psqueue(K, P)::in, K::out, P::out) is det.

    % create an ordered association list from priority search queue
    %
:- func to_ord_assoc_list(psqueue(K, P)) = assoc_list(K, P).
:- pred to_ord_assoc_list(psqueue(K, P)::in, assoc_list(K, P)::out) is det.

    % remove element with specific key from priority queue
    %
:- func delete(K, psqueue(K, P)) = psqueue(K, P) is semidet.
:- pred delete(K::in, psqueue(K, P)::in, psqueue(K, P)::out) is semidet.

    % remove element with specific key from priority queue, call error/1 if
    % element is not present
    %
:- func det_delete(K, psqueue(K, P)) = psqueue(K, P) is det.
:- pred det_delete(K::in, psqueue(K, P)::in, psqueue(K, P)::out) is det.

    % insert key with specified priority into priority search queue
    %
:- func insert(K, P, psqueue(K, P)) = psqueue(K, P) is semidet.
:- pred insert(K::in, P::in, psqueue(K, P)::in, psqueue(K, P)::out) is semidet.

    % As above, will call error/1 if the key is already present
    %
:- func det_insert(K, P, psqueue(K, P)) = psqueue(K, P) is det.
:- pred det_insert(K::in, P::in, psqueue(K, P)::in, psqueue(K, P)::out) is det.

    % adjust priority of specified element. The old priority is given as an
    % argument to the adjustment function
    %
:- func adjust(func(P) = P, K, psqueue(K, P)) = psqueue(K, P) is semidet.
:- pred adjust((func(P) = P)::in, K::in, psqueue(K, P)::in, psqueue(K, P)::in)
    is semidet.

   % adjust priority of specified element. The old priority is given as an
    % argument to the adjustment function, call error/1 if element is not
    % present
    %
:- func det_adjust(func(P) = P, K, psqueue(K, P)) = psqueue(K, P) is det.
:- pred det_adjust((func(P) = P)::in, K::in, psqueue(K, P)::in,
                  psqueue(K, P)::out) is det.

    % lookup the priority of the specified element
    %
:- func lookup(K, psqueue(K, P)) = P is semidet.
:- pred lookup(K::in, psqueue(K, P)::in, P::out) is semidet.

    % lookup the priority of the specified element, call error/1 if element is
    % not present
    %
:- func det_lookup(K, psqueue(K, P)) = P is det.
:- pred det_lookup(K::in, psqueue(K, P)::in, P::out) is det.

    % return the size of the priority search queue as the number of elements
    %
:- func size(psqueue(K, P)) = int is det.
:- pred size(psqueue(K, P)::in, int::out) is det.

    % true if the priority search queue respects the semi heap properties,
    % i.e., 1) the top element has the highest priority and 2) for each node of
    % the loser tree, the priority of the loser is higher or equal to the
    % priorities in the subtree from which the loser originates
    %
:- pred is_semi_heap(psqueue(K, P)::in) is semidet.

    % true if the priority search queue respects the search tree properties,
    % i.e., for each node the keys in the left subtree are smaller as or equal
    % to the split key and the keys in the right subtree are larger than the
    % split key
    %
:- pred is_search_tree(psqueue(K, P)::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

:- type psqueue(K, P) --->
      void
    ; winner(K, P, ltree(K, P), K).

:- type t_ltree_size == int.

:- type ltree(K, P) --->
      start
    ; loser(t_ltree_size, K, P, ltree(K, P), K, ltree(K, P)).

%---------------------------------------------------------------------------%

% create empty psqueue
psqueue.init = PSQ :-
        psqueue.init(PSQ).

psqueue.init(void).

% check for empty psqueue
psqueue.is_empty(void).


    % create singleton psqueue
    %
:- pred singleton(K::in, P::in, psqueue(K, P)::out) is det.
:- func singleton(K, P) = psqueue(K, P).

singleton(K, P) = Res :-
    singleton(K, P, Res).

singleton(K, P, PSQ) :-
    PSQ = winner(K, P, start, K).


    % extract maximal (highest priority) key
    %
:- func max_key(psqueue(K, P)) = K is semidet.
:- pred max_key(psqueue(K, P)::in, K::out) is semidet.

max_key(PSQ) = K :-
        max_key(PSQ, K).

max_key(PSQ, MaxKey) :-
        PSQ = winner(_, _, _, MaxKey).


    % play tournament to combine two priority search queues, see Ralf Hinze's
    % paper for explanantion
    %
:- func tournament(psqueue(K, P), psqueue(K, P)) = psqueue(K, P) is det.
:- pred tournament(psqueue(K, P)::in, psqueue(K, P)::in, psqueue(K, P)::out)
    is det.

    % generate specialized code for integral priorities
    %
:- pragma type_spec(tournament/3, P = int).

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
    (
      LTree = start,
      Res = void
    ;
      LTree = loser(_, LK, LP, T, SplitKey, U),
      ( LK `leq` SplitKey ->
          T1 = winner(LK, LP, T, SplitKey),
          T2 = second_best(U, Key),
          Res = tournament(T1, T2)
      ;
          T1 = second_best(T, SplitKey),
          T2 = winner(LK, LP, U, Key),
          Res = tournament(T1, T2)
      )
    ).

to_ord_assoc_list(PSQ) = Res :-
    to_ord_assoc_list(PSQ, Res).

to_ord_assoc_list(PSQ, AList) :-
    ( psqueue.del_min(PSQ, K, P, PSQ0) ->
        to_ord_assoc_list(PSQ0, AList0),
        AList = [K - P | AList0]
    ;
        AList = []
    ).

del_min(PSQ, MinKey, MinPrio, NewPSQ) :-
    PSQ = winner(MinKey, MinPrio, L, MaxKey),
    NewPSQ = second_best(L, MaxKey).

peek(PSQ, MinKey, MinPrio) :-
    PSQ = winner(MinKey, MinPrio, _, _).

det_peek(PSQ, MinKey, MinPrio) :-
    ( peek(PSQ, MinKey0, MinPrio0) ->
        MinKey = MinKey0,
        MinPrio = MinPrio0
    ;
        unexpected($file, $pred, "priority search queue is empty")
    ).

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

%---------------------------------------------------------------------------%

    % get min view of priority search queue
    %
:- func min_view(psqueue(K, P)) = t_min_view(K, P) is det.

min_view(PSQ) = Res :-
    (
      PSQ = void, Res = emtpy
    ;
      PSQ = winner(Key, Prio, LTree, MaxKey),
      Res = min(Key, Prio, second_best(LTree, MaxKey))
    ).

    % get tournament view of priority search queue
    %
:- func tournament_view(psqueue(K, P)) = t_tournament_view(K, P) is det.

tournament_view(PSQ) = Res :-
    (
      PSQ = void,
      Res = emptySet
    ;
      PSQ = winner(K, P, LTree, MaxKey),
      (
        LTree = start, Res = singleton(K, P)
      ;
        LTree = loser(_, LK, LP, TL, SplitKey, TR),
        ( LK `leq` SplitKey ->
            Res = tournament_between(winner(LK, LP, TL, SplitKey),
                                     winner(K, P, TR, MaxKey))
        ;
            Res = tournament_between(winner(K, P, TL, SplitKey),
                                     winner(LK, LP, TR, MaxKey))
        )
      )
    ).


    % get tree view of priority search queue
    %
:- func tree_view(ltree(K, P)) = t_tree_view(K, P) is det.

tree_view(LTree) = Res :-
    (
      LTree = start, Res = leaf
    ;
      LTree = loser(_, LK, LP, LL, SplitKey, LR),
      Res = node(LK, LP, LL, SplitKey, LR)
    ).


lookup(K, PSQ) = lookup_tv(K, tournament_view(PSQ)).

lookup(K, PSQ, P) :-
    P = lookup(K, PSQ).

det_lookup(K, PSQ) = Res :-
    ( Res0 = lookup(K, PSQ) ->
        Res = Res0
    ;
        unexpected($file, $pred, "element to look-up is not present in\
                  priority search queue")
    ).

det_lookup(K, PSQ, P) :-
    P = det_lookup(K, PSQ).

:- func lookup_tv(K, t_tournament_view(K, P)) = P is semidet.
lookup_tv(K, TV) = Res :-
    (
      TV = singleton(Key, Prio),
      Key = K,
      Res = Prio
    ;
      TV = tournament_between(TL, TR),
      TL = winner(_, _, _, MaxKey1),
      ( K `leq` MaxKey1 ->
          Res = lookup(K, TL)
      ;
          Res = lookup(K, TR)
      )
    ).


adjust(F, K, PSQ) = adjust_tv(F, K, tournament_view(PSQ)).

adjust(F, K, PSQ, PSQ0) :-
    PSQ0 = adjust(F, K, PSQ).

det_adjust(F, K, PSQ) = Res :-
    ( PSQ0 = adjust(F, K, PSQ) ->
        Res = PSQ0
    ;
        unexpected($file, $pred, "element to adjust not present in priority\
                  search queue")
    ).

det_adjust(F, K, PSQ, PSQ0) :-
    PSQ0 = det_adjust(F, K, PSQ).

:- func adjust_tv(func(P) = P, K, t_tournament_view(K, P)) = psqueue(K, P)
    is semidet.
adjust_tv(Func, K, TV) = Res :-
    (
      TV = emptySet, Res = void
    ;
      TV = singleton(Key, Prio),
      ( K = Key ->
          Res = psqueue.singleton(Key, Func(Prio))
      ;
          Res = psqueue.singleton(Key, Prio)
      )
    ;
      TV = tournament_between(TL, TR),
      TL = winner(_, _, _, MaxKey1),
      ( K `leq` MaxKey1 ->
          Res = tournament(adjust(Func, K, TL), TR)
      ;
          Res = tournament(TL, adjust(Func, K, TR))
      )
    ).

insert(IK, IP, PSQ) = insert_tv(IK, IP, tournament_view(PSQ)).

insert(IK, IP, PSQ, PSQ0) :-
    PSQ0 = insert(IK, IP, PSQ).

det_insert(IK, IP, PSQ) = Res :-
        ( Res0 = insert_tv(IK, IP, tournament_view(PSQ)) ->
            Res = Res0
        ;
            unexpected($file, $pred, "error in deterministic insert")
        ).

det_insert(IK, IP, PSQ, PSQ0) :-
    PSQ0 = det_insert(IK, IP, PSQ).

:- func insert_tv(K, P, t_tournament_view(K, P)) = psqueue(K, P) is semidet.
insert_tv(IK, IP, TV) = Res :-
    (
      TV = emptySet, Res = psqueue.singleton(IK, IP)
    ;
      TV = singleton(Key, Prio),
      compare(CMP, IK, Key),
      (
        CMP = (<), Res = tournament(psqueue.singleton(IK, IP),
                                    psqueue.singleton(Key, Prio))
      ;
        CMP = (=), Res = psqueue.singleton(IK, IP)
      ;
        CMP = (>), Res = tournament(psqueue.singleton(Key, Prio),
                                    psqueue.singleton(IK, IP))
      )
    ;
      TV = tournament_between(T1, T2),
      T1 = winner(_, _, _, MaxKey1),
      T2 = winner(_, _, _, _),
      ( IK `leq` MaxKey1 ->
          Res = tournament(insert(IK, IP, T1), T2)
      ;
          Res = tournament(T1, insert(IK, IP, T2))
      )
    ).

delete(DK, PSQ) = delete_tv(DK, tournament_view(PSQ)).

delete(DK, PSQ, PSQ0) :-
    PSQ0 = delete(DK, PSQ).

det_delete(DK, PSQ) = Res :-
    ( PSQ0 = delete(DK, PSQ) ->
        Res = PSQ0
    ;
        unexpected($file, $pred, "element not in priority search queue")
    ).

det_delete(DK, PSQ, PSQ0) :-
    PSQ0 = det_delete(DK, PSQ).

:- func delete_tv(K, t_tournament_view(K, P)) = psqueue(K, P) is semidet.
delete_tv(DK, TV) = Res :-
    (
      (
        TV = emptySet, Res = void
      ;
        TV = singleton(Key, Prio),
        ( DK = Key ->
            Res = void
        ;
            Res = psqueue.singleton(Key, Prio)
        )
      )
    ;
      TV = tournament_between(TL, TR),
      TL = winner(_, _, _, MaxKey1),
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
      PSQ = winner(_, _, LTree, _),
      Size = ltree_size(LTree)
    ).

size(PSQ) = Res :-
    size(PSQ, Res).

:- func ltree_size(ltree(K, P)) = t_ltree_size is det.
ltree_size(LTree) = Res :-
    (
      LTree = start, Res = 0
    ;
      LTree = loser(Res, _, _, _, _, _)
    ).

%---------------------------------------------------------------------------%
% smart constructors
%---------------------------------------------------------------------------%

:- func construct_leaf = ltree(K, P).
construct_leaf = start.

:- func construct_node(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P).
construct_node(Key, Prio, L, SplitKey, R) = Res :-
    Size = 1 + ltree_size(L) + ltree_size(R),
    Res = loser(Size, Key, Prio, L, SplitKey, R).


%---------------------------------------------------------------------------%
% balancing functions for weight balanced trees
%---------------------------------------------------------------------------%

    % balance factor, must be over 3.75 (see Ralf Hinze's paper)
    %
:- func balance_omega = t_ltree_size.
balance_omega = 4.

:- func balance(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func balance_left(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func balance_right(K, P, ltree(K, P), K, ltree(K, P)) = ltree(K, P) is det.
:- func single_left(K, P, ltree(K, P), K, t_tree_view(K, P)) = ltree(K, P)
    is det.
:- func single_right(K, P, t_tree_view(K, P), K, ltree(K, P)) = ltree(K, P)
    is det.
:- func double_left(K, P, ltree(K, P), K, t_tree_view(K, P)) = ltree(K, P)
    is det.
:- func double_right(K, P, t_tree_view(K, P), K, ltree(K, P)) = ltree(K, P)
    is det.

balance(Key, Prio, L, SplitKey, R) = Res :-
    SizeL = ltree_size(L),
    SizeR = ltree_size(R),
    ( (SizeR + SizeL) < 2 ->
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
            Res = construct_node(K1, P1,
                                 construct_node(K2, P2, T1, S1, T2), S2, T3)
        ;
            Res = construct_node(K2, P2,
                                 construct_node(K1, P1, T1, S1, T2), S2, T3)
        )
    ;
        unexpected($file, $pred, "error in single left rotation")
    ).

single_right(K1, P1, TVL, S2, T3) = Res :-
    ( TVL = node(K2, P2, T1, S1, T2) ->
        ( ( compare(CMP0, K2, S1), CMP0 = (>), P1 `leq` P2 ) ->
            Res = construct_node(K1, P1, T1, S1,
                                 construct_node(K2, P2, T2, S2, T3))
        ;
            Res = construct_node(K2, P2, T1, S1,
                                 construct_node(K1, P1, T2, S2, T3))
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

%---------------------------------------------------------------------------%
% test predicates for correct implementation of psqueue
%---------------------------------------------------------------------------%

is_semi_heap(PSQ) :-
    (
      PSQ = void
    ;
      PSQ = winner(_, Prio, LTree, _),
      all_keys_larger_ltree(Prio, LTree),
      all_nodes_loser_prio(LTree)
    ).

:- pred all_keys_larger_ltree(P::in, ltree(K, P)::in) is semidet.
all_keys_larger_ltree(Prio, LTree) :-
    (
      LTree = start
    ;
      LTree = loser(_, _, LP, LT, _, RT),
      Prio `leq` LP,
      all_keys_larger_ltree(Prio, LT),
      all_keys_larger_ltree(Prio, RT)
    ).

:- pred min_prio_loser_tree(ltree(K, P)::in, P::out) is semidet.
:- pred min_prio_loser_tree(ltree(K, P)::in, P::in, P::out) is det.

min_prio_loser_tree(LTree, MinPrio) :-
    LTree = loser(_, _, Prio, TL, _, TR),
    min_prio_loser_tree(TL, Prio, MinPrio1),
    min_prio_loser_tree(TR, Prio, MinPrio2),
    ( MinPrio1 `leq` MinPrio2 ->
        MinPrio = MinPrio1; MinPrio = MinPrio2).

min_prio_loser_tree(LTree, CurrMin, MinPrio) :-
    (
      LTree = start, MinPrio = CurrMin
    ;
      LTree = loser(_, _, Prio, TL, _, TR),
      ( CurrMin `leq` Prio ->
          min_prio_loser_tree(TL, CurrMin, MinPrio1),
          min_prio_loser_tree(TR, CurrMin, MinPrio2)
      ;
          min_prio_loser_tree(TL, Prio, MinPrio1),
          min_prio_loser_tree(TR, Prio, MinPrio2)
      ),
      ( MinPrio1 `leq` MinPrio2 ->
          MinPrio = MinPrio1; MinPrio = MinPrio2)
    ).

:- pred all_nodes_loser_prio(ltree(K, P)::in) is semidet.
all_nodes_loser_prio(LTree) :-
    (
      LTree = start
    ;
      LTree = loser(_, K, Prio, TL, SplitKey, TR),
      ( K `leq` SplitKey ->
          min_prio_loser_tree(TL, Prio, MinPrio)
      ;
          min_prio_loser_tree(TR, Prio, MinPrio)
      ),
      compare(CMP, Prio, MinPrio),
      CMP = (=),
      all_nodes_loser_prio(TL),
      all_nodes_loser_prio(TR)
    ).

is_search_tree(PSQ) :-
    (
      PSQ = void
    ;
      PSQ = winner(_, _, LTree, _),
      all_search_keys(LTree)
    ).

:- pred all_search_keys(ltree(K, P)::in) is semidet.
all_search_keys(LTree) :-
    (
      LTree = start
    ;
      LTree = loser(_, K, _, TL, _, TR),
      max_key_loser_tree(TL, MaxKeyL),
      min_key_loser_tree(TR, MinKeyR),
      MaxKeyL `leq` K,
      K `leq` MinKeyR,
      all_search_keys(TL),
      all_search_keys(TR)
    ).


:- pred min_key_loser_tree(ltree(K, P)::in, K::out) is semidet.
:- pred min_key_loser_tree(ltree(K, P)::in, K::in, K::out) is det.

min_key_loser_tree(LTree, MinKey) :-
    LTree = loser(_, Key, _, TL, _, TR),
    min_key_loser_tree(TL, Key, MinKey1),
    min_key_loser_tree(TR, Key, MinKey2),
    ( MinKey1 `leq` MinKey2 ->
        MinKey = MinKey1; MinKey = MinKey2).

min_key_loser_tree(LTree, CurrMin, MinKey) :-
    LTree = start, MinKey = CurrMin
    ;
    LTree = loser(_, Key, _, TL, _, TR),
    ( CurrMin `leq` Key ->
        min_key_loser_tree(TL, CurrMin, MinKey1),
        min_key_loser_tree(TR, CurrMin, MinKey2)
    ;
        min_key_loser_tree(TL, Key, MinKey1),
        min_key_loser_tree(TR, Key, MinKey2)
    ),
    ( MinKey1 `leq` MinKey2 ->
        MinKey = MinKey1; MinKey = MinKey2).

:- pred max_key_loser_tree(ltree(K, P)::in, K::out) is semidet.
:- pred max_key_loser_tree(ltree(K, P)::in, K::in, K::out) is det.

max_key_loser_tree(LTree, MaxKey) :-
    LTree = loser(_, Key, _, TL, _, TR),
    min_key_loser_tree(TL, Key, MaxKey1),
    min_key_loser_tree(TR, Key, MaxKey2),
    ( MaxKey2 `leq` MaxKey1 ->
        MaxKey = MaxKey2; MaxKey = MaxKey1).

max_key_loser_tree(LTree, CurrMax, MaxKey) :-
    LTree = start, MaxKey = CurrMax
    ;
    LTree = loser(_, Key, _, TL, _, TR),
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