/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/rtlex_closure.p
 > Purpose:
 > Author:          John Gibson, Mar 10 1988 (see revisions)
 */

;;; ------------ CONSTRUCTING RUN=TIME LEXICAL CLOSURES ----------------------

#_INCLUDE 'declare.ph'

constant
        procedure ( Sys$-Flush_procedure, Sys$-Cons_closure, )
    ;

;;; -------------------------------------------------------------------------

section $-Sys;

define Cons_lex_closure(clos_template) -> clos;
    lvars clos, clos_template, _addr, _lim, _size;
    ;;; copy the closure template
    if clos_template <@(w) _system_end then
        ;;; system procedure -- recreate from scratch
        Cons_closure(clos_template!PD_CLOS_NFROZ) -> clos;
        clos_template!KEY -> clos!KEY;
        clos_template!PD_PROPS -> clos!PD_PROPS;
        clos_template!PD_UPDATER -> clos!PD_UPDATER;
        clos_template!PD_FLAGS -> clos!PD_FLAGS;
        clos_template!PD_NARGS -> clos!PD_NARGS;
        clos_template!PD_CLOS_NFROZ -> clos!PD_CLOS_NFROZ;
        clos_template!PD_CLOS_PDPART -> clos!PD_CLOS_PDPART;
    else
        @@(w)[clos_template!PD_LENGTH] -> _size;
        Get_store(_size) -> clos;
        _moveq(_size, clos_template@POPBASE, clos@POPBASE) -> ;
        ;;; adjust the PD_EXECUTE field for copied closure
        Adjust_pdr_exec(@@(w){clos, clos_template}, clos);
    endif;

    ;;; fill the idents into the frozvals
    clos@PD_CLOS_FROZVALS -> _lim;
    _lim@(w)[clos!PD_CLOS_NFROZ] -> _addr;
    while _addr >@(w) _lim do
        -> _addr--!(w) -> _addr
    endwhile;

#_IF DEF CACHEFLUSH
    Flush_procedure(clos);
#_ENDIF

    if (clos!PD_UPDATER ->> clos_template)
    and clos_template!PD_FLAGS _bitst _:M_PD_CLOSURE then
        Cons_lex_closure(clos_template) -> clos!PD_UPDATER
    endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Mar 21 1994
        Changed to recreate system closures from scratch with Cons_closure
--- Robert John Duncan, Feb 11 1991
        Added cache flush
--- John Gibson, Dec  6 1989
        Changes for new pop pointers
 */
