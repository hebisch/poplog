/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/init_idval.p
 > Purpose:
 > Author:          John Gibson, Feb  4 1989 (see revisions)
 */

;;; ------------ INITIALISING VALUES OF IDENTIFIERS ----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

global constant
        procedure isundef
    ;

section $-Sys;

weak constant
        Undef$-normal_u, procedure Undef$-normal_p
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys;

define Set_undef_clos(clos);
    lvars clos;
    _:M_PD_CLOSURE _biset _:M_PD_CLOS_PROTECT _biset _:M_PD_CLOS_UNDEF
        -> clos!PD_FLAGS;
    if clos!PD_UPDATER then
        clos!PD_FLAGS -> clos!PD_UPDATER!PD_FLAGS
    endif;
enddefine;

define Make_idval_active(id);
    lvars id, clos, val, _mult;
    if id!ID_IDENTPROPS _bitst _:M_ID_OPERATOR then
        1
    else
        _pint(id!ID_NUM_ATTRIBUTE)
    endif -> _mult;
    fast_idval(id) -> val;
    consclosure(identfn, fast_repeat _mult times val endfast_repeat, _mult)
                                            ->> clos -> fast_idval(id);
    _0 -> clos!PD_NARGS;
    consclosure(erasenum, val, _mult fi_+ 1, 2) -> clos!PD_UPDATER;
    _int(_mult) -> clos!PD_UPDATER!PD_NARGS;
    if isundef(val) then Set_undef_clos(clos) endif
enddefine;

define Make_idval_pdr(id);
    lvars id, clos, val = fast_idval(id);
    consclosure(Exec_nonpd, val, 1) ->> clos -> fast_idval(id);
    if isundef(val) then Set_undef_clos(clos) endif
enddefine;

define Make_idval_undef(id);
    lvars id, _idprops = id!ID_IDENTPROPS;
    ;;; these must only be referred to with weakref
    ;;; so poplink generates undefs for them
    if _idprops _bitst _:M_ID_PROCEDURE then
        weakref Undef$-normal_p
    else
        weakref Undef$-normal_u
    endif -> fast_idval(id);
    if _idprops _bitst _:M_ID_ACTIVE then
        ;;; need an active undef
        chain(id, Make_idval_active)
    endif
enddefine;


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, May 15 1989
        Included ident.ph
--- John Williams, Feb 15 1989
        -Set_undef_clos- checks updater exists before setting flags
--- John Williams, Feb 14 1989
        Made -Set_undef_clos- not lconstant 'cos used in SRC * LISPCORE.P
 */
