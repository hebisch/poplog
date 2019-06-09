/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/src/xt_timeout.p
 > Purpose:         X Toolkit - timeout functions
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt  =>
                        fast_XtAppAddTimeOut,
                        fast_XtRemoveTimeOut,
                        XptImportIntervalId,
;


;;; record constructor
define lconstant Cons_IntervalId_rec(_ptr,appcon,interval,proc,arg) -> id;
    lvars _ptr appcon interval proc arg id dep;
    Cons_XptDescriptor(_ptr,XDT_INTERVALID,interval,XD_T_DEP_LEN) -> id;
    id!XD_DEPENDENTS -> dep;

    proc -> dep!XD_T_PROC;
    arg -> dep!XD_T_CLIENT;
    false -> dep!XD_T_APPCON;

    if appcon and (Get_Descriptor(XDT_APPCONTEXT,appcon) ->> appcon) then
        appcon -> dep!XD_T_APPCON;
        appcon!XD_DEPENDENTS -> appcon;
        id :: appcon!XD_AC_TIMEOUTS -> appcon!XD_AC_TIMEOUTS;
    endif;
enddefine;

define fast_XtAppAddTimeOut(appcon,interval,proc,arg) -> id;
    lvars appcon interval proc arg _ptr id;

    X_apply(appcon,interval,proc,arg, _4, _extern XtAppAddTimeOut) -> _ptr;
    Cons_IntervalId_rec(_ptr,appcon,interval,proc,arg) -> id;
enddefine;

define fast_XtRemoveTimeOut(id);
    lvars id dep;
    unless id!XP_PTR == _NULL then
        X_apply(id,_1,_extern XtRemoveTimeOut) ->;
        if Descriptor(XDT_INTERVALID,id!XP_PTR) ->> id then
            id!XD_DEPENDENTS -> dep;
            if dep!XD_T_APPCON then
                Del_item(id,dep!XD_T_APPCON!XD_DEPENDENTS@XD_AC_TIMEOUTS);
            endif;
            Kill_XptDescriptor(id);
        endif;
    endunless;
enddefine;

;;; import a intervalid descriptor
define lconstant ImportIntervalIdDesc(_ptr);
    lvars _ptr;
    Descriptor(XDT_INTERVALID,_ptr) or
        ;;; make a descriptor - don't know any associated info!
        Cons_IntervalId_rec(_ptr,false,false,false,false)
enddefine;


;;; public implicit access routine
define XptImportIntervalId(_ptr);
    lvars _ptr;
    Checkr_exptrclass_ptr(_ptr) -> _ptr;
    if _ptr == _NULL then
        false
    else
        Register(ImportIntervalIdDesc(_ptr));
    endif;
enddefine;

define updaterof XptImportIntervalId(_ptr) -> _ptr;
    lvars _ptr;
    if _ptr then
        unless XptDataType(_ptr) == XDT_INTERVALID then
            mishap(_ptr,1,'IntervalId NEEDED');
        endunless;
    else
        null_external_ptr -> _ptr;
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Roger Evans, Jul  1 1991 removed destroy actions from IntervalId's
--- Roger Evans, Jun 28 1991 altered for freelists
--- Roger Evans, Jun 28 1991 removed callback code, added import routine
--- Roger Evans, Nov  4 1990 fixed bug in Remove proc
--- Roger Evans, Oct 11 1990 Much revised
 */
