/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 *  File:           C.all/plog/lib/logic.pl
 *  Purpose:        "sugar prolog" demonstration program
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * LOGIC
 *  Related Files:  text file $usepop/pop/plog/lib/logic.tch
 */

:- library(simplepop), library(useful).
:- dopop("vars lastline;").

get_lines(W) :- prolog_eval(prolog_readline('* '), W1),
   rest_lines(W1,W).

rest_lines([],[]) :- !.
rest_lines(W,[]) :- prolog_eval(termin,W), !.
rest_lines(W,W) :- append(X,['.'],W), !.
rest_lines(W,W) :- append(X,['?'],W), !.
rest_lines(W,W2) :- get_lines(W1), append(W,W1,W2).

doprocess :-
        get_lines(W),
        !,
        process(W).

/* Process one line of input */

process([]) :-
        last_line_blank,
        !,
        see('$usepop/pop/plog/lib/logic.tch'),
        get0(C),
        tutor(C),
        see(user),
        recordline(non_blank),
        nlwrite('PRESS RETURN TWICE TO GET NEXT LESSON'),
        !,
        fail.

process([]) :-
        !,
        recordline([]),
        fail.

;;;process([input, complete]) :-
;;;        nlwrite('END OF EXECUTION').

process([show, '.']) :-
        !,
        recordline(non_blank),
        (fact(Y); nlwrite('no facts stored'), fail), !,
        fact(X),
        nlwrite(X),
        fail.

process([delete, '.']) :-
        !,
        recordline(non_blank),
        (fact(XX); nlwrite('nothing to delete'), fail), !,
        lastfact(X),
        nlwrite(['I assume you mean delete', X]),
        retract(fact(X)),
        nlwrite('ok - deleted'),
        !,
        fail.

process([delete|S]) :-
        !,
        recordline(non_blank),
        (append(X, ['.'], S); nlwrite('missing full stop'), fail), !,
        parse(X, T), !,
        (retract(fact(T)), nlwrite('ok - deleted'); nlwrite('not known')),
        !,
        fail.

process([bye|S]) :-
        !,
        nlwrite('BYE'),
        prolog_eval(apply(valof(sysexit))).

process(['BYE'|S]) :- process(['bye'|S]).

process([save|[F|G]]) :-
        !,
        recordline(non_blank),
        tell(F),
        writefacts,
        told,
        tell(user),
        !,
        nlwrite(['ok - knowledge stored as', F]),
        fail.

process([restore|[F|G]]) :-
        !,
        recordline(non_blank),
        reconsult(F),
        nlwrite('ok - have restored knowledge from disc'),
        !,
        process([show, '.']).

process(S) :-
        recordline(non_blank),
        append(X, ['.'], S), !,
        parse(X, T), !,
        (fact(T), nlwrite('already known'); assert(fact(T)), nlwrite(ok)),
        !,
        fail.

process(S) :-
        recordline(non_blank),
        append(X, ['?'], S), !,
        parse(X, T), !,
        answer(T),
        fail.

process(S) :-
        recordline(non_blank),
        nlwrite('sorry - input not understood'),
        nlwrite('might have been either a question or a statement'),
        fail.

/* Send out information from the help file */

tutor(126).
tutor(26) :- seen.
tutor(C) :-
        put(C),
        get0(CC),
        tutor(CC).

lastfact(X) :-
        setuptofindlast,
        fact(Y),
        notelast(Y),
        fail.

lastfact(X) :-
        retract(lastwas(X)).

notelast(X) :-
        retract(lastwas(Y)),
        assert(lastwas(X)),
        !.

setuptofindlast :-
        fact(X),
        assert(lastwas(X)),
        !.


writefacts :-
        fact(F),
        write(fact(F)),
        write('.'),
        nl,
        fail.

writefacts :-
        !.


nlwrite(X) :-
        flatten(X, Y),
        writelist(Y),
        !.

flatten([[list|A]|B], C) :-
        flatten(A, AA),
        flatten(B, BB),
        append(['('|A], [')'|BB], C).

flatten([A|B], C) :-
        flatten(A, FA),
        flatten(B, FB),
        append(FA, FB, C).

flatten([], []).

flatten(X, [X]).

writelist(['('|B]) :- !,
        write('('),
        writelist(B).

writelist([A|[')'|[B]]]) :- !,
        write(A),
        writelist([')' | B]).

writelist([A|B]) :- !,
        write(A),
        write(' '),
        writelist(B).

writelist([]) :-
        nl.

unwrite(X, A, [A|B], B) :-
        var(X),
        !,
        X = A.

unwrite([A|B], [AA|BB], C, E) :-
        !,
        unwrite(A, AA, C, D),
        !,
        unwrite(B, BB, D, E).

unwrite(A, A, B, B) :- !.

answer(Q) :-
        rewrite(Q, QQ),
        !,
        (solve(QQ, []),
                unwrite(QQ, QQQ, [x, y, z, xx, yy, zz], _),
                nlwrite(['yes -'|QQQ]);
                nlwrite('no (more) solutions')).

solve(X, Y) :-          /* second arg of solve is list of current goals? */
        var(X),
        !,
        fail.

solve(X, [Y|Z]) :-
        member(X, Z),
        !,
        unwrite(X, XX, [x, y, z, xx, yy, zz], _),
        nlwrite(['Recursive goal', XX]),
        fail.

solve([X, and, Y], L) :- !,
        solve(X, L),
        solve(Y, L).

solve(Q, L) :-
        fact(F),
        rewrite(F, Q).

solve(Q, L) :-
        fact(F),
        rewrite(F, [Q, if, R]),
        solve(R, [Q|L]).

rewrite(A, B) :- rewrite(A, B, L).

rewrite([A|B], [C|D], L) :- !,
        rewrite(A, C, L),
        rewrite(B, D, L).
rewrite(V, VV, L) :- member(V,[x,y,z,who,what,xx,yy,zz,which,someone,something]),
                        member([V|VV],L),!.
rewrite(A, A, L).

parse(X, Z) :-
        parse1(X, Y),
        !,
        parse2(Y, Z).

parse1(['('|X], [[list|CC]|DD]) :-
        !,
        append(AA, [')'|BB], X),
        parse1(AA, CC),
        parse1(BB, DD).         /* was parse2 BUG ?? */

parse1([')'|X], Y) :-
        !,
        fail.

parse1([X|Y], [X|Z]) :-
        parse1(Y, Z).

parse1([], []).

parse2(X, [Y, if, Z]) :-
        append(YY, [if|ZZ], X),
        not(member(and, YY)),
        parse2(YY, Y),
        parse2(ZZ, Z).

parse2(X, [Y, and, Z]) :-
        append(YY, [and|ZZ], X),
        parse2(YY, Y),
        parse2(ZZ, Z).

parse2(X, X).

go :-
        dopop("78 -> poplinewidth;"),
        nlwrite('THE LOGIC PROGRAMMING SYSTEM'),
        nlwrite(''),
        nlwrite('PRESS THE RETURN BUTTON FOR HELP'),
        (retract(last(X, Y)); true),
        recordline([]),
        see(user),
        repeat,
        doprocess.

/* Keeping track of last lines */

recordline([]) :- !,
   dopop('"blank" -> lastline;').
recordline(X) :-
   dopop('"non_blank" -> lastline;').

last_line_blank :-
   prolog_eval(valof(lastline), blank).

fact(X) :- fail.

;;; ?- go.

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 14 1993
        Replaced prolog_safer*eadline with prolog_readline
 */
