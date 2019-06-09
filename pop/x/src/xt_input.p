/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/src/xt_input.p
 > Purpose:         X Toolkit - input events
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'
#_INCLUDE '../../src/io.ph'


section $-Sys$-Xt  =>
                        fast_XtAppAddInput,
                        fast_XtRemoveInput,
                        XptImportInputId,
                        XptInputIdToDevice,
;


;;; record constructor
define lconstant Cons_InputId_rec(_ptr,appcon,source,cond,proc,arg) -> id;
    lvars _ptr appcon source cond proc arg id dep;
    Cons_XptDescriptor(_ptr,XDT_INPUTID,source!D_OPEN_NAME,XD_I_DEP_LEN) -> id;
    id!XD_DEPENDENTS -> dep;

    source -> dep!XD_I_DEVICE;
    cond -> dep!XD_I_COND;
    proc -> dep!XD_I_PROC;
    arg -> dep!XD_I_CLIENT;
    false -> dep!XD_I_APPCON;

    if appcon and (Get_Descriptor(XDT_APPCONTEXT,appcon) ->> appcon) then
        appcon -> dep!XD_I_APPCON;
        appcon!XD_DEPENDENTS -> appcon;
        id :: appcon!XD_AC_INPUTS -> appcon!XD_AC_INPUTS;
    endif;
enddefine;

define fast_XtAppAddInput(appcon,source,cond,proc,arg) -> id;
    lvars appcon, source, cond, proc, arg, _ptr, id, _fd;

    device_os_channel(source) -> _fd;
#_IF DEF VMS
    if cond /== XtInputReadMask then
        mishap(appcon, source, cond, 3, 'CONDITION FOR ALTERNATE INPUT MUST BE XtInputReadMask')
    endif;
    ;;; XptAppAddInput uses a timer to poll for input on terminals & mailboxes
    X_apply(appcon, _fd, 0, proc, arg, _5, _extern XptAppAddInput) -> _ptr;
    if _ptr == _NULL then
        mishap(appcon, source, 2, 'CANNOT ADD DEVICE AS ALTERNATE INPUT')
    endif;
#_ELSE
    X_apply(appcon, _fd, cond, proc, arg, _5, _extern XtAppAddInput) -> _ptr;
#_ENDIF

    Cons_InputId_rec(_ptr,appcon,source,cond,proc,arg) -> id;
enddefine;

define fast_XtRemoveInput(id);
    lvars id dep;
    unless id!XP_PTR == _NULL then
        X_apply(id, _1, #_IF DEF VMS _extern XptRemoveInput
                        #_ELSE       _extern XtRemoveInput
                        #_ENDIF) ->;
        if Descriptor(XDT_INPUTID,id!XP_PTR) ->> id then
            id!XD_DEPENDENTS -> dep;
            if dep!XD_I_APPCON then
                Del_item(id,dep!XD_I_APPCON!XD_DEPENDENTS@XD_AC_INPUTS);
            endif;
            Kill_XptDescriptor(id);
        endif;
    endunless;
enddefine;

;;; import a inputid descriptor
define lconstant ImportInputIdDesc(_ptr);
    lvars _ptr;
    Descriptor(XDT_INPUTID,_ptr) or
        ;;; make a descriptor - don't know any associated info!
        Cons_InputId_rec(_ptr,false,false,false,false,false)
enddefine;


;;; public implicit access routine
define XptImportInputId(_ptr);
    lvars _ptr;
    Checkr_exptrclass_ptr(_ptr) -> _ptr;
    if _ptr == _NULL then
        false
    else
        Register(ImportInputIdDesc(_ptr));
    endif;
enddefine;

define updaterof XptImportInputId(_ptr) -> _ptr;
    lvars _ptr;
    if _ptr then
        unless XptDataType(_ptr) == XDT_INPUTID then
            mishap(_ptr,1,'InputId NEEDED');
        endunless;
    else
        null_external_ptr -> _ptr;
    endif;
enddefine;


define XptInputIdToDevice(id);
    lvars id desc;
    Checkr_exptrclass_ptr(id) -> ;
    if (Get_Descriptor(XDT_INPUTID,id) ->> desc) then
        desc!XD_DEPENDENTS!XD_I_DEVICE;
    else
        mishap(id,1,'CANNOT GET DEVICE FROM InputId');
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Roger Evans, Jul  1 1991 remved destroy actions from IntervalId's
--- Roger Evans, Jun 28 1991 altered for freelists
--- Roger Evans, Jun 28 1991 removed callback code, added import code
        added XptInputIdToDevice
--- John Gibson, Feb 11 1991
        Added VMS mods
--- Roger Evans, Nov  4 1990 fixed bug in Remove proc
--- Roger Evans, Oct 11 1990 Much revised
 */
