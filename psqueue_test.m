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


:- pred test_psqueue_paper_ex(psqueue(int, string)::in, psqueue(int, string)::out) is det.
test_psqueue_paper_ex -->
    psqueue.insert(8, "Warren"),
    psqueue.insert(2, "Erik"),
    psqueue.insert(7, "Richard"),
    psqueue.insert(5, "Simon"),
    psqueue.insert(4, "Charles"),
    psqueue.insert(6, "Mary"),
    psqueue.insert(3, "Phil"),
    psqueue.insert(1, "Lennart").

main(!IO) :-
    io.print("empty test: ", !IO),
    ( test_psqueue_empty ->
        io.print("ok", !IO)
    ;
        io.print("nok", !IO)
    ),
    io.nl(!IO),
    io.print("paper example test: ", !IO),
    ( test_psqueue_paper_ex(psqueue.init, PSQ_EX),
        io.print(PSQ_EX, !IO)
    ),
    io.nl(!IO),
    (
      at_most(4, PSQ_EX, AList3),
      io.print(AList3, !IO)
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
      insert(4, "Homer", PSQ0, PSQ1),
      insert(1, "Lisa", PSQ1, PSQ2),
      insert(2, "Bart", PSQ2, PSQ3),
      insert(0, "Maggie", PSQ3, PSQ4),
      insert(3, "Marge", PSQ4, PSQ5),
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
    io.nl(!IO)
    .
