/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedclipboard.p
 > Purpose:         Cut/Paste selection/clipboard
 > Author:          Jonathan Meyer, May 30 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

include xt_constants.ph;
include xpt_coretypes.ph;

section $-xved;

XptLoadProcedures xvedclipboard [^^XPW_EXLIBS]
lvars
    XpwGetSelection(w,location,receive_p,client) :void,
    XpwSetSelection(w,location,string,nbytes,lose_p,client) :void,
;

lconstant macro (
    SELN_LOCATION   = 1,
    SELN_VALUE      = 2,
    SELN_LOSE_P     = 3,
    );

lvars
    ;;;                         LOCATION    VALUE   LOSE_P
    Primary     = writeable {% 1,           false, identfn %},
    Clipboard   = writeable {% 'CLIPBOARD', false, identfn %},
    ;

define lconstant copy_vedstring(string);
    lvars string;
    subvedstring(1, datalength(string), string)
enddefine;

define lconstant Selection(seln) -> result;
    lvars seln;
    dlvars fetch_done = false, result;

    define lconstant receive_callback(w, client, call);
        lvars w, client, call;
        exacc :XptString call -> result;
        true -> fetch_done;
    enddefine;

    returnif(seln(SELN_VALUE) ->> result) (copy_vedstring(result) -> result);

    exacc[fast] raw_XpwGetSelection(xveddummyshell, seln(SELN_LOCATION),
                    XptExportCallbackCached(receive_callback, false, true));
    until fetch_done do
        fast_XtAppProcessEvent(xvedappcontext, XtIMAll)
    enduntil
enddefine;
;;;
define updaterof Selection(string, lose_p, seln);
    lvars seln, string, lose_p;

    define lconstant lose_callback(w, seln, call);
        lvars w, seln, call;
        false -> seln(SELN_VALUE);
        seln(SELN_LOSE_P)();
        identfn -> seln(SELN_LOSE_P)
    enddefine;

    ;;; clobber existing one before assigning new lost procedure
    seln(SELN_LOSE_P)();
    lose_p -> seln(SELN_LOSE_P);

    if string then copy_vedstring(string) -> seln(SELN_VALUE) endif;

    exacc[fast] raw_XpwSetSelection(
        xveddummyshell, seln(SELN_LOCATION),
        if string then
            ->XptCoerceTmpString(string), datalength(string),
            XptExportCallbackCached(lose_callback, seln, true)
        else
            null_external_ptr, 0, null_external_ptr, 0
        endif);
enddefine;

define global vars active vvedclipboard;
    xveddummyshell and (Selection(Primary) or Selection(Clipboard))
enddefine;
;;;
define updaterof active vvedclipboard(string);
    lvars string, lose_p, seln = Primary;

    ;;; not much  point in allowing locations other than "CLIPBOARD", since
    ;;; the base procedure can't access them
    if string == "CLIPBOARD" then
        () -> string;
        Clipboard -> seln
    endif;

    if string.isprocedure then
        (), string
    else
        string, identfn
    endif -> (string, lose_p);

    returnunless(xveddummyshell);

    if string then string sys_>< '' -> string endif;
    string, lose_p -> Selection(seln);
    if string then string -> vvedlastclipboard endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  8 1995
        Made it use subvedstring to copy strings
--- Jonathan Meyer, Sep 23 1993
        fixed updater of Selection to allow -false- string.
--- John Gibson, Jan 12 1993
        popcom*piler -> subsystem etc
--- John Gibson, Jan 19 1992
        Rewritten so that vvedclipboard caches its own selections
        (so it'll deal with dstring selections).
--- John Gibson, Aug 21 1991
        Changed to use callbacks rather than actions. Now uses separate
        "lose" procedures for primary and clipboard.
--- John Gibson, Jul 15 1991
        Made selections be owned by xveddummyshell
--- John Gibson, Jul 11 1991
        Added null to end of 'CLIPBOARD'
--- John Gibson, Jul 10 1991
        Put back running of old lost action by updater of vvedclipboard
        -- otherwise, setting a new selection runs the new lost action,
        i.e. tries to remove itself at the same time as setting itself.
--- Jonathan Meyer, Jul  8 1991
        Added optional argument to updaterof vvedclipboard taking location
        argument
--- Jonathan Meyer, Jul  3 1991
        C code now calls lost action for previous selection.
        Also, the CLIPBOARD selection is no longer set.
--- John Gibson, Jul  2 1991
        Made updater of vvedclipboard run 'lost' action for previous
        selection
 */
