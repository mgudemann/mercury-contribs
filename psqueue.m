
:- module psqueue.
:- interface.

:- type psqueue(K, P).

:- func psqueue.init = psqueue(K, V).
:- pred psqueue.init(psqueue(K, P)::out) is det.

:- pred psqueue.is_empty(psqueue(K, P)::in) is semidet.

:- func psqueue.max_key(psqueue(K, P)) = K is semidet.
:- pred psqueue.max_key(psqueue(K, P)::in, K::out) is semidet.

:- implementation.

:- type psqueue(K, P) --->
	  void
	; winner(K, P, ltree(K, P), K).

:- type ltree(K, P) --->
	  start
	; loser(K, P, ltree(K, P), K, ltree(K, P)).


% create empty psqueue
psqueue.init = PSQ :-
	psqueue.init(PSQ).

psqueue.init(void).


% check for empty psqueue
psqueue.is_empty(void).


% extract maximal (highest priority) key
max_key(PSQ) = K :-
	max_key(PSQ, K).

max_key(PSQ, MaxKey) :-
	PSQ = winner(_, _, _, MaxKey).