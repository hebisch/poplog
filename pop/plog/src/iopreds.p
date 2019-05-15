/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/iopreds.p
 > Purpose:         Prolog: support for I/O predicates
 > Author:          Rob Duncan & Simon Nichols, Nov  3 1987 (see revisions)
 > Related Files:
 */


section prolog;

constant
    procedure ( prolog_printq, closeout, format, tell, prolog_rename,
        closein, see, prolog_writeq, prolog_print, prolog_display, );

vars
    procedure readterm,
    current_output,
    current_input,
    readenv,
;

weak vars
    procedure ( prolog_read_command, ),
;

;;; =======================================================================

;;; on_tty:
;;;     call a procedure where -cucharin-, -cucharout- are bound to the
;;;     terminal (i.e. -charin- and -charout-). This is for the DEC-10
;;;     "tty" predicates.

define on_tty(pd) with_props false;
    lvars   pd;
    dlocal  cucharin = charin, cucharout = charout;
    pd();
enddefine;

;;; prolog_cucharin :
;;;     reads a single character from the current input stream but uses
;;;     the Prolog prompt and returns CTRL-Z at end of file.

define prolog_cucharin();
    dlocal popprompt = prolog_read_prompt;
    if dup(cucharin()) == termin then -> ; `\^Z` endif;
enddefine;

;;; prolog_get:
;;;     reads the next non-space character from the input

define prolog_get(input) -> c;
    lvars   procedure input, c;
    dlocal  popprompt = prolog_read_prompt;
    repeat
        input() -> c;
        returnif(c == termin or c fi_> 32);
    endrepeat;
enddefine;

;;; prolog_skip:
;;;     read characters from the input up to -item-

define prolog_skip(item, input) -> c;
    lvars   item, procedure input, c;
    dlocal  popprompt = prolog_read_prompt;
    input() -> c;
    returnif(isprologvar(item));
    until c == termin or c == item do
        input() -> c;
    enduntil;
enddefine;

;;; prolog_read:
;;;     read a term from the current input

define prolog_read();
    dlocal
        popprompt = prolog_read_prompt,
        weakref prolog_read_command = #_< identfn(%false%) >_#,
    ;
    readterm();
enddefine;

;;; prolog_portray:
;;;     call portray/1 to print -term-.

define prolog_portray(/* term */) with_nargs 1;
    SAVE;
    prolog_push_continuation("prolog_own_exit_on_success", 1);
    unless dup(prolog_own_invoke(/* term, */ portray\/1)) then
        RESTORE;
    endunless;
enddefine;

;;; prompt/2

define prompt\/2(Old, New);
    lvars Old, New;
    returnunless(prolog_unify(Old, consword(prolog_read_prompt)));
    if isprologvar(prolog_deref(New) ->> New) then
        prolog_assign(New, Old);
    elseif isword(New) or isstring(New) or New == [] then
        New sys_>< nullstring -> prolog_read_prompt;
    else
        mishap(New, 1, 'ATOM NEEDED FOR NEW PROMPT');
    endif;
    chain(prolog_apply_continuation);
enddefine;

endsection;     /* prolog */

PROLOG

:- module prolog.

:- dynamic portray/1.

:- inline((
        get0(C) :-
            prolog_eval(apply(valof(prolog_cucharin)), C)
    )).

:- inline((
        ttyget0(C) :-
            prolog_eval(on_tty(valof(prolog_cucharin)), C)
    )).

:- inline((
        get(C) :-
            prolog_eval(prolog_get(valof(cucharin)), C),
            quote(C) =\= valof(termin)
    )).

:- inline((
        ttyget(C) :-
            prolog_eval(prolog_get(valof(charin)), C),
            quote(C) =\= valof(termin)
    )).

:- inline((
        skip(C) :-
            prolog_eval(prolog_skip(quote(C), valof(cucharin)), C),
            quote(C) =\= valof(termin)
    )).

:- inline((
        ttyskip(C) :-
            prolog_eval(prolog_skip(quote(C), valof(charin)), C),
            quote(C) =\= valof(termin)
    )).

:- inline((
        put(C) :-
            prolog_eval(cucharout(C))
    )).

:- inline((
        ttyput(C) :-
            prolog_eval(charout(C))
    )).

:- inline((
        tab :-
            prolog_eval(cucharout(32))
    )).

:- inline((
        ttytab :-
            prolog_eval(charout(32))
    )).

:- inline((
        tab(N) :-
            prolog_eval(sp(N))
    )).

:- inline((
        ttytab(N) :-
            prolog_eval(on_tty(N, valof(sp)))
    )).

:- inline((
        nl :-
            prolog_eval(cucharout(10))
    )).

:- inline((
        ttynl :-
            prolog_eval(charout(10))
    )).

:- inline((
        nl(N) :-
            prolog_eval(nl(N))
    )).

:- inline((
        ttynl(N) :-
            prolog_eval(on_tty(N, valof(nl)))
    )).

:- inline((
        ttyflush :-
            prolog_eval(sysflush(valof(pop_charout_device)))
    )).

:- inline((
        read(T) :-
            prolog_eval(apply(valof(prolog_read)), T)
    )).

:- inline((
        read(T, Env) :-
            prolog_eval(apply(valof(prolog_read)), T),
            prolog_eval(valof(readenv), Env)
    )).

:- inline((
        write(T) :-
            prolog_eval(prolog_write(quote(T)))
    )).

:- inline((
        writeq(T) :-
            prolog_eval(prolog_writeq(quote(T)))
    )).

:- inline((
        display(T) :-
            prolog_eval(prolog_display(quote(T)))
    )).

:- inline((
        print(T) :-
            prolog_eval(prolog_print(quote(T)))
    )).

:- inline((
        printq(T) :-
            prolog_eval(prolog_printq(quote(T)))
    )).

:- inline((
        format(Fmt) :-
            prolog_eval(format0(quote(Fmt)))
    )).

:- inline((
        format(Fmt, Args) :-
            prolog_eval(format(quote(Fmt, Args)))
    )).

:- inline((
        see(Stream) :-
            prolog_eval(see(quote(Stream)))
    )).

:- inline((
        seeing(Stream) :-
            prolog_eval(valof(current_input), Stream)
    )).

:- inline((
        seen :-
            prolog_eval(closein(valof(current_input)))
    )).

:- inline((
        tell(Stream) :-
            prolog_eval(tell(quote(Stream)))
    )).

:- inline((
        telling(Stream) :-
            prolog_eval(valof(current_output), Stream)
    )).

:- inline((
        told :-
            prolog_eval(closeout(valof(current_output)))
    )).

:- inline((
        close(Stream) :-
            prolog_eval(closein(quote(Stream))),
            prolog_eval(closeout(quote(Stream)))
    )).

vread(X, Env) :-
    read(X, Env).

write(X, Env) :-
    (   prolog_bindvars(Env) ->
        write(X),
        fail
    ;   !,
        fail
    ).
write(_, _).

vwrite(X, Env) :-
    write(X, Env).

prolog_bindvars([]) :-
    !.
prolog_bindvars([X=X|Env]) :-
    !,
    prolog_bindvars(Env).
prolog_bindvars([_|Env]) :-
    prolog_bindvars(Env).


rename(Oldname, Newname) :-
    prolog_eval(prolog_rename(quote(Oldname, Newname))).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Changed to new inline strategy. Consult and library predicates
        moved out to "compile.p".
--- Robert John Duncan, Apr  8 1992
        Added format/[1,2]
--- Simon Nichols, Oct  8 1991
        Added close/1 which closes the specified input or output stream.
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Simon Nichols, May 28 1991
        Added printq/1, a Prolog interface to -prolog_printq- in "write.p".
--- Simon Nichols, Oct 17 1990
        Changed print/1 to interface to -prolog_print- in "write.p".
--- Simon Nichols, Oct 17 1990
        Added read/2 and write/2 as synonyms for vread/2 and vwrite/2.
--- Simon Nichols, Jul 17 1990
        Changed read/1 to dlocal prolog_read_command to identfn(%false%).
        This change is required by the new implementation of commands.
--- Simon Nichols, Jun 28 1990
        Fixed get/1 to return <false> on EOF.
--- Rob Duncan, Jun 12 1990
        Fixed -vwrite/2- for the case where some variables in the
        environment have become bound.
--- Rob Duncan, Aug  8 1989
    Sectionised and added #_INCLUDEs for POPC;
    added new predicates -vread/2-, -vwrite/2-;
    changed POP-11 definitions to use the revised define forms: -predicate-,
    -inline- and -optimisable-.
--- Rob Duncan, Jul 20 1989
    Changed ttyflush/0 to call -sysflush- on the charout device rather
    than do charout(0).
--- Rob Duncan, Sep  5 1988
    Rewrote to use -prolog_pdr- and -prolog_inline_pdr-
--- Rob Duncan, Aug 31 1988
    Replaced -vednullstring- with -nullstring-
 */
