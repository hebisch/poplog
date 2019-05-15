/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:        C.all/plog/src/procedures.p
 > Purpose:     Prolog: operations on Prolog procedures
 > Author:      Jonathan Laventhol (see revisions)
 */

section prolog;

;;; prolog_invisible:
;;;     the value of a Prolog predicate is a procedure, but possibly
;;;     wrapped in various "invisible" closures for tracing, debugging
;;;     etc. This property records those wrapper procedures currently
;;;     known to the system

define vars prolog_invisible =
    newproperty([], 8, false, "tmparg");
enddefine;

;;; add_wrapper:
;;;     add an "invisible" wrapper to a procedure

define add_wrapper(/* wrapper, proc, args ..., n */) -> clos with_nargs 3;
    lvars clos = writeable consclosure(/* wrapper, proc, args ..., n */);
    true -> prolog_invisible(pdpart(clos));
    recursive_front(pdprops(frozval(1, clos))) -> pdprops(clos);
enddefine;

;;; del_wrapper:
;;;     delete a wrapper from a procedure

define del_wrapper(wrapper, proc) -> proc;
    lvars wrapper, proc;
    if isclosure(proc) and prolog_invisible(pdpart(proc)) then
        if pdpart(proc) == wrapper then
            frozval(1, proc) -> proc;
        else
            del_wrapper(wrapper, frozval(1, proc)) -> frozval(1, proc);
        endif;
    endif;
enddefine;

;;; unwrapped:
;;;     get the true value of a procedure, stripped of any wrappers.

define unwrapped(proc) -> proc;
    lvars proc;
    while isclosure(proc) and prolog_invisible(pdpart(proc)) do
        frozval(1, proc) -> proc;
    endwhile;
enddefine;
;;;
define updaterof unwrapped(new, proc) -> proc;
    lvars new, proc;
    if isclosure(proc) and prolog_invisible(pdpart(proc)) then
        new -> unwrapped(frozval(1, proc)) -> frozval(1, proc);
    else
        new -> proc;
    endif;
enddefine;

;;; proc_name:
;;;     get the name of a Prolog procedure (the front of the pdprops)

define proc_name(proc) -> name;
    lvars proc, name = recursive_front(pdprops(unwrapped(proc)));
enddefine;
;;;
define updaterof proc_name(name, proc);
    lvars name, proc;
    unwrapped(proc) -> proc;
    lvars props = pdprops(proc);
    if ispair(props) then conspair(name, Back(props)) -> name endif;
    name -> pdprops(proc);
enddefine;

;;; proc_clauses:
;;;     get the clauses of a Prolog procedure (the back of the pdprops)

define proc_clauses(proc) -> clauses;
    lvars proc, clauses = [], props = pdprops(unwrapped(proc));
    if ispair(props) then Back(props) -> clauses endif;
enddefine;
;;;
define updaterof proc_clauses(clauses, proc);
    lvars clauses, proc;
    unwrapped(proc) -> proc;
    lvars name = recursive_front(pdprops(proc));
    conspair(name, clauses) -> pdprops(proc);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Stripped down to the bare essentials: most stuff moved out to other
        files. Added more disciplined support for "invisible" wrappers.
--- Simon Nichols, Nov 12 1992
        Made closures created by -prolog_valof- writeable.
--- Simon Nichols, Feb 12 1992
        There is no longer a global variable -usage_env- declared in
        "gencode.p". Instead, -make_usage_env- returns the usage environment
        for a clause. The procedures -group_unit_clauses-, -prolog_define-
        and -compile_one_clause- (now renamed -comp_one_clause-) have been
        modified accordingly.
--- Robert Duncan, Jan 27 1992
        Changed -converter_to_contn- to check that the continuation argument
        is a procedure and then use "fast_apply" to call it, to avoid
        creating another stack frame.
--- Simon Nichols, Aug 31 1990
        Changed the updater of -prolog_valof- to ensure a predicate
        record exists for the Prolog procedure.
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - moved out define form -prolog_pdr- to "define.p";
    - changed -prolog_valof- to react to -pop_pas_mode-;
    - added -prolog_define_dynamic- for new, dynamic predicates;
    - added calls to -writeable- when adding new links to clause lists.
--- Rob Duncan, Nov 14 1988
    Fixed -group_unit_clauses- to test -tl(l)- instead of -l- in the first
    call to -comp_clause-. This was previously not recognising the last
    clause, creating some unnecessary choice points.
--- Rob Duncan, Oct 21 1988
    Changed -prolog_abolish- to stop it assigning to the predicate record
    of the abolished predicate
--- Rob Duncan, Sep  5 1988
    Added -prolog_abolish- and the define form -prolog_pdr-
--- Rob Duncan, Mar 16 1988
    Renamed from xbinter.p and substantially rewritten
--- Simon Nichols, Nov  2 1987
    Changed -this_section- to -prolog_section-.
--- John Gibson, Sep 20 1987
    (1) For a group of unit clauses inside -group_unit_clauses-, made []
    be matched by IFNOT_UNIFY rather than IFNOT_ATOM (prevents predicates
    with a (sometimes nil) list in an argument position being fragmented).
    (2) Rearranged -prolog_assert- and -prolog_define- so that they pass
    a code-planting procedure to -comp_predicate-, rather than starting a
    procedure with -compstart- and finishing with -compend- (see comments
    in xbcomp.p).
--- John Gibson, Mar 12 1987
    Changed -prolog_define- to use -comp_unit_clause_group- for a contiguous
    sequence of constant unit clauses, matching against all or part of the list
    of clauses produced for -termsfrom-.
--- R. J. Duncan, S. C. Nichols 22 Dec. 1986
    Modified the list of clause procedures used by -prolog_interpreter- to
    include a dummy item at the head of the list. This fixed a bug in RETRACT
    reported by SD.
    Involved rewriting -insert_element-, -delete_element- and -prolog_interpreter-
    plus other minor changes as marked.

 */
