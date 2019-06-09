/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/src/xt_procedure.p
 > Purpose:         XptProcedure key
 > Author:          Roger Evans, Jul  8 1990 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'
#_INCLUDE '../../src/external.ph'


section $-Sys$-Xt =>    XptImportProcedure,
;

;;; Create a procedure descriptor given an exfunc_closure
define Cons_Procedure_from_closure(proc,props) -> rec;
    lvars proc props rec;
    ;;; make descriptor record
    Cons_XptDescriptor(proc@EFC_CODE, XDT_PROCEDURE, props,false) -> rec;

    ;;; keep closure in DEPENDENTS slot
    proc -> rec!XD_DEPENDENTS;
enddefine;

;;; Create a basic procedure descriptor given an extptr to a procedure
define Cons_Procedure_rec(proc,props) -> desc;
    lvars proc props desc;

    ;;; make an external closure which sets _in_X_call true (i.e. nonzero)
    ;;; (nb: this is localised by external calls etc.)
    Cons_exfunc_closure(proc, 1, false, ident _in_X_call);

    Cons_Procedure_from_closure((),props) -> desc;

    ;;; register desc against original proc address as well as exfunc address
    desc -> Descriptor(proc!XP_PTR);
enddefine;


define XptImportProcedure(ptr);
    lvars ptr, desc, props;
    if Checkr_exptrclass_ptr(ptr) == _NULL then
        false
    else
        unless Get_Descriptor(XDT_PROCEDURE,ptr) ->> desc then
            if class_attribute(ptr!KEY,"external_ptr_props") then
                ptr!XP_PROPS
            else
                false
            endif -> props;
            if not(props) and ptr!KEY == external_ptr_key then
                ;;; Since this is a conversion procedure it's liable to get
                ;;; passed 'fixed' exptrs (ie fixed meaning the same ones for
                ;;; every call in a particular place). Thus if it's an ordinary
                ;;; external_ptr with false props, use a copy of it not the
                ;;; original.
                copy(ptr) -> ptr
            endif;
            Cons_Procedure_rec(ptr, props) -> desc
        endunless;
        Register(desc)
    endif;
enddefine;
;;;
define updaterof XptImportProcedure(ptr) -> ptr;
    lvars ptr;
    if ptr then
        if isexfunc_closure(ptr) then
            if ptr!EFC_ARG_DEST == ident _in_X_call then
                ;;; we made this closure, so safe to return underlying proc
                ptr!EFC_FUNC -> ptr;
            endif;
        elseunless XptDataType(ptr) == XDT_PROCEDURE then
            mishap(ptr,1,'EXFUNC_CLOSURE OR XtProcedure NEEDED');
        endif;
    else
        null_external_ptr -> ptr;
    endif;
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- John Gibson, Sep 24 1992
        Made XptImportProcedure give Cons_Procedure_rec a copy of
        its argument.
--- Roger Evans, Jun 28 1991
        now registers proc descriptors agains underlying c proc address
        (as well as exfunc closure address). Also export 'undoes' exfunc
        closure that are just _in_X_call wrappers. Also altered for freelists
--- Roger Evans, Jun 28 1991 changed import to allow exfunc_closures
--- John Gibson, Mar 13 1991
        Variable Sys$-Xt$-DisableXAsync changed to Sys$- _in_X_call
--- Roger Evans, Jan 26 1991 added inlcude for external.ph
--- John Gibson, Jan 15 1991
        Replaced c*lass_spec with class_attribute
        Moved X_apply to src/extern_ptr.p
--- Roger Evans, Nov 16 1990 added Cons_Procedure_from_closure
--- Roger Evans, Oct 11 1990 Much revised
 */
