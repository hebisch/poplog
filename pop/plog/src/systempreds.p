/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/plog/src/systempreds.p
 > Purpose:         Prolog: predicates for tailoring the Prolog system
 > Author:          Rob Duncan & Simon Nichols, Nov  3 1987 (see revisions)
 */

include subsystem

section prolog;

constant procedure ( alias, bad_goal, );

vars prolog_exceptions;

;;; ========================================================================

;;; prolog_restart/0:
;;;     full restart of the Prolog system

define prolog_restart\/0();
    clearstack();
    chainto(setpop,
        procedure;
            "top" -> subsystem;
            syssetup();
            setpop();
        endprocedure);
enddefine;

;;; prolog_reinitialise:
;;;     reinitialise the Prolog system by calling -syssetup-.

define prolog_reinitialise();
    dlocal pop_nobanner = true;
    syssetup();
enddefine;


;;; prolog_save_image:
;;;     creates a Prolog saved image using either -syssave- or
;;;     -sys_lock_system-.

define prolog_save_image(name, attributes);
    lvars   name, attributes, attribute;
    dlocal  pop_nobanner;
    prolog_full_deref(attributes) -> attributes;
    for attribute in attributes do
        unless lmember(attribute, [lock share init banner]) then
            mishap(attribute, 1, 'save/2: INVALID ATTRIBUTE');
        endunless;
    endfor;
    if lmember("lock", attributes) then
        lvars mode = lmember("share", attributes) and 1 or 0;
        unless pop_record_writeable then
            ;;; make all closures writeable, in case there are some
            ;;; marked writeable which haven't been recorded
            mode fi_|| 2:100 -> mode;
        endunless;
        sys_lock_system(name, mode, word_string(name));
    else
        syssave(name);
    endif;
    if dup() then
        unless lmember("banner", attributes) then
            true -> pop_nobanner;
        endunless;
        if lmember("init", attributes) then
            syssetup();
        elseunless pop_nobanner then
            sys_subsystems_init(SS_BANNER);
        endif;
    endif;
enddefine;


;;; prolog_system_procedure:
;;;     true if the predicate -fn/arity- is a system predicate.

define prolog_system_procedure(fn, arity);
    lvars fn, arity;
    not(isassignable(alias(fn, arity)));
enddefine;

endsection;     /* prolog */


;;; ====  System Predicates Defined in Prolog  ============================

PROLOG

:- module prolog.

/*  Abort and halt  */

abort :-
    prolog_eval(apply(valof(interrupt))).

halt :-
    prolog_eval(apply(valof(sysexit))).


/*  Reinitialise  */

reinitialise :-
    prolog_eval(apply(valof(prolog_reinitialise))).


/*  Version messages  */

version :-
    prolog_eval(prolog_version([])).

version(X) :-
    var(X), !,
    prolog_eval(bad_goal(quote(X), version, 1)).
version([]) :- !,
    prolog_eval(prolog_version([])).
version(Mes) :-
    prolog_eval(prolog_version(quote(Mes))).


/*  Setting and Examining System Variables  */

prolog_no_clauses :-
    prolog_no_clauses(on).

prolog_clauses :-
    prolog_no_clauses(off).

prolog_no_clauses(X) :-
    var(X), !,
    (prolog_evaltrue(valof(prolog_no_clauses)) -> X = on; X = off).
prolog_no_clauses(on) :- !,
    prolog_setq(prolog_no_clauses, valof(true)).
prolog_no_clauses(off) :- !,
    prolog_setq(prolog_no_clauses, valof(false)).


prolog_syspredicates(X) :-
    var(X), !,
    (prolog_evaltrue(valof(prolog_syspredicate)) -> X = on; X = off).
prolog_syspredicates(on) :- !,
    prolog_setq(prolog_syspredicate, valof(true)).
prolog_syspredicates(off) :-
    prolog_setq(prolog_syspredicate, valof(false)).


prolog_gctrace(X) :-
    var(X), !,
    (prolog_evaltrue(valof(popgctrace)) -> X = on; X = off).
prolog_gctrace(on) :- !,
    prolog_setq(popgctrace, 1).
prolog_gctrace(off) :-
    prolog_setq(popgctrace, valof(false)).


prolog_gcratio(Reading) :-
    var(Reading), !,
    Reading is valof(popgcratio).
prolog_gcratio(Writing) :-
    integer(Writing),
    prolog_setq(popgcratio, Writing).


prolog_memused(Reading) :-
    Reading is valof(popmemused).


prolog_memlim(Reading) :-
    var(Reading), !,
    Reading is valof(popmemlim).
prolog_memlim(Writing) :-
    integer(Writing),
    prolog_setq(popmemlim, Writing).


prolog_area_lim(Reading) :-
    var(Reading), !,
    Reading is valof(pop_prolog_lim).
prolog_area_lim(Writing) :-
    integer(Writing),
    prolog_setq(pop_prolog_lim, Writing).


prolog_area_size(Reading) :-
    var(Reading), !,
    Reading is valof(pop_prolog_size).
prolog_area_size(Writing) :-
    integer(Writing),
    prolog_setq(pop_prolog_size, Writing).


prolog_callstack_lim(Reading) :-
    var(Reading), !,
    Reading is valof(pop_callstack_lim).
prolog_callstack_lim(Writing) :-
    integer(Writing),
    prolog_setq(pop_callstack_lim, Writing).


prolog_error_handling(X) :-
    var(X), !,
    (prolog_evaltrue(valof(prolog_exceptions)) -> X = on; X = off).
prolog_error_handling(on) :- !,
    prolog_setq(prolog_exceptions, valof(true)).
prolog_error_handling(off) :-
    prolog_setq(prolog_exceptions, valof(false)).


/*  Miscellaneous Utilities  */

prolog_system_predicate(Fn, Arity) :-
    atom(Fn),
    integer(Arity),
    prolog_evaltrue(prolog_system_procedure(quote(Fn), quote(Arity))).

trimcore :-
    prolog_eval(apply(valof(sysgarbage))).

prolog_lock_code :-
    prolog_eval(apply(valof(sysgarbage))),
    prolog_eval(apply(valof(sys_lock_heap))).

prolog_unlock_code :-
    prolog_eval(apply(valof(sys_unlock_heap))).

save(Name) :-
    save(Name, _).

save(Name, RestoreFlag) :-
    (integer(RestoreFlag) ; var(RestoreFlag)), !,
    (prolog_evaltrue(syssave(quote(Name))) ->
        RestoreFlag = 1
    ;   RestoreFlag = 0
    ).
save(Name, Attributes) :-
    prolog_evaltrue(prolog_save_image(quote(Name), quote(Attributes))).

restore(Name) :-
    prolog_eval(sysrestore(quote(Name))).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 13 1996
        Included revised version of prolog_error_handling/1.
--- Robert John Duncan, May 17 1995
        Changed prolog_save_image to be more careful with writeable
        closures
--- Robert John Duncan, Jul 15 1993
        Various definitions moved out to other files.
--- John Gibson, Jan 18 1993
        Replaced s*witch_subsystem_to and c*urrent_subsystem_name with
        sys_compiler_subsystem etc.
--- Robert John Duncan, Apr  9 1992
        Introduced format/[1,2]
--- Simon Nichols, Dec 13 1991
        Added prolog_area_size/1.
--- Simon Nichols, Aug 19 1991
        Renamed restart/0 prolog_restart/0 because of conflict with Flex
        KSL predicate of the same name.
--- Robert John Duncan, Jun 28 1991
        Added restart/0.
--- Simon Nichols, Jun 26 1991
        Changed -prolog_save_image- to dlocal -pop_nobanner-.
--- Robert John Duncan, Jun  6 1991
        Replaced '%nobanner' with -pop_nobanner-
--- Simon Nichols, Nov  1 1990
        Changed -prolog_reinitialise- to locally cons '%nobanner' to the
        front of -poparglist- (rather than overwriting it).
        Fixed -prolog_save_image- to respect %nobanner flag.
--- Simon Nichols, Oct 11 1990
        Changed save/2 to accept a list of attributes for the saved image.
--- John Williams, Oct  9 1990
        %nobanner instead of =nobanner
--- John Williams, Oct  4 1990
        Now uses -syssetup-
--- Simon Nichols, Sep 12 1990
        Added prolog_abolish_command/1.
--- Simon Nichols, Aug 31 1990
        Changed -predicate_info- to report two additional attributes:
        "undefined" -- predicates which have been declared but not defined)
        "nonprolog" -- predicates defined in a language other than Prolog.
--- Simon Nichols, Jul 19 1990
        Added predicate expand_term/2 to support clause pre-processing.
--- Simon Nichols, Jul 18 1990
        Undid the last change, i.e. removed prolog_macros/1, ved/0 and
        ved/1. These are no longer needed because commands have been
        re-implemented (they are not macros).
--- Simon Nichols, Feb  6 1990
    - added predicate prolog_macros/1 to enable users to disable macros;
    - added predicates ved/0 and ved/1 to enable users to call VED when
        macros are disabled.
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - moved out -prolog_initcomp- and -prolog_version- to "startup.p";
    - added localisation of -prolog_syspredicate- and -prolog_no_clauses-
        in -declare_dynamic-, -declare_system- and -declare_user- to
        ensure that consistent predicate records are created;
    - added new module predicates: global/1, import/1, export/1.
--- Simon Nichols, Nov  7 1988
    Changed warning message in -declare_dynamic-.
 */
