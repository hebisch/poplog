/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/consclosure.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROCEDURE
 */

;;; ------------------ CONSTRUCTING CLOSURES ----------------------------

#_INCLUDE 'declare.ph'

section $-Sys;

constant
        procedure (Cons_closure, Flush_procedure),
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys => consclosure, sys_grbg_closure;

    ;;; Chains of free closures upto length 16 (chained thru PD_PROPS field)
constant
    _free_clos_tab = _INIT_NONPOP_STRUCT(word[17]);


    /* Construct a closure
    */
define consclosure(_n) -> clos;
    lvars clos, p, _ptr, _lim, _clos, _n;
    Check_integer(_n, 0);
    _int(_n) -> _n;

    if _n _lt _17 and iscompound(_free_clos_tab!(w)[_n] ->> _clos) then
        ;;; use a free one
        _clos!PD_PROPS -> _free_clos_tab!(w)[_n]    ;;; next in chain
    else
        ;;; get closure without frozvals or pdpart filled in -- keep in a
        ;;; non-pop variable until all fields safely assigned
        Cons_closure(_n) -> _clos;
        ;;; initialise header (PD_EXECUTE and PD_LENGTH already filled in)
        procedure_key -> _clos!KEY;
        _n -> _clos!PD_CLOS_NFROZ;
    endif;

    false ->> _clos!PD_PROPS -> _clos!PD_UPDATER;
    _:M_PD_CLOSURE -> _clos!PD_FLAGS;   ;;; says its a closure
    ;;; 16:FF makes pdnargs(clos) = pdnargs(pdpart)-nfroz by default
    _16:FF -> _clos!PD_NARGS;

    ;;; fill in frozvals
    _clos@PD_CLOS_FROZVALS -> _lim;
    _lim@(w)[_n] -> _ptr;
    while _ptr >@(w) _lim do
        () -> _ptr--!(w) -> _ptr        ;;; next frozval off stack
    endwhile;

    ;;; check pdpart is a procedure
    Check_procedure(() ->> p);
    p -> _clos!PD_CLOS_PDPART;

    ;;; now safe to put in a pop var
    _clos -> clos;

#_IF DEF CACHEFLUSH
    Flush_procedure(clos);
#_ENDIF

    if p!PD_UPDATER then
        copy(clos) ->> _clos -> clos!PD_UPDATER;
        p!PD_UPDATER -> _clos!PD_CLOS_PDPART
    endif
enddefine;

    /*  Construct a protected closure
    */
define Consclos_protect() -> clos;
    lvars clos;
    consclosure(/*nfroz*/) -> clos;
    _:M_PD_CLOSURE _biset _:M_PD_CLOS_PROTECT -> clos!PD_FLAGS;
    if clos!PD_UPDATER then
        _:M_PD_CLOSURE _biset _:M_PD_CLOS_PROTECT -> clos!PD_UPDATER!PD_FLAGS
    endif
enddefine;

    /*  Execute a closure. Called by closure code planted in -partapply- for
        closures that have a large number of frozvals.
    */
define Exec_closure(clos);
    lvars clos, _offs, _limit;
    ;;; put frozvals on stack
    @@PD_CLOS_FROZVALS[_0] -> _offs;
    @@PD_CLOS_FROZVALS[clos!PD_CLOS_NFROZ] -> _limit;
    while _offs _lt _limit do
        _CHECKUSER;
        clos!(w){_offs};
        @@(w){_offs}++ -> _offs
    endwhile;
    ;;; execute pdpart
    fast_chain(clos!PD_CLOS_PDPART)
enddefine;

define sys_grbg_closure(clos);
    lvars clos, _n;
    if isprocedure(clos) and clos!PD_FLAGS _bitst _:M_PD_CLOSURE
    and clos >=@(w) _system_end
    and (clos!PD_CLOS_NFROZ ->> _n) _lt _17 then
        _free_clos_tab!(w)[_n] -> clos!PD_PROPS;
        clos -> _free_clos_tab!(w)[_n]
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 20 1996
        Added sys_grbg_closure
--- John Gibson, Oct 18 1994
        free*1closures -> _free_1*closures
--- John Gibson, Apr  8 1992
        Added -free*1closures-
--- Robert John Duncan, Feb 11 1991
        Added cache flush
--- John Gibson, May  8 1988
        Moved -partapply- to autoloadable library.
--- John Gibson, May  1 1988
        Replaced -partapply- with -consclosure- as the main means of
        constructing closures; C.<machine>/src/partapply.p now replaced
        with C.<machine>/src/closure_cons.p, which contains the procedure
        -Cons_closure- to construct a an uninitialised closure for a
        given number of frozvals.
 */
