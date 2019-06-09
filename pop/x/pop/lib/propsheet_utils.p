/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/propsheet_utils.p
 > Purpose:         Old *PROPSHEET file
 > Author:          Jonathan Meyer, Sep  5 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

    /*********************************************************
     *  NOTE: This file is no longer used by LIB * PROPSHEET *
     *********************************************************/

section $-propsheet_utils;

uses xt_widget;
uses XtN;

include xpt_generaltypes.ph;

/* a small piece of syntax for setting a fixed arglist again and again */

/* "fills" an arglist up to point N. */

define lconstant set_arg_list(arglist, argcount) -> (arglist, argcount);
    lvars arglist argcount i;
    shadowclass lconstant XptArgList [fast, nc, prefix fast_nc_];

    fast_for i from argcount by -1 to 1 do
        -> fast_nc_subscrXptArgList(i, arglist);
    endfast_for;
enddefine;
/* syntax construct for setting arglists:

    XptArgs mylist { name: value }
*/

define constant syntax XptArgs;
    lvars arglist_count = 0, tmpvar = false, lenvar = false, arglist;
    itemread() -> arglist;
    pop11_need_nextreaditem("{")->;
    until pop11_try_nextreaditem("}") do
        if pop11_try_nextreaditem("%") then
            ;;; stacklength() -> lenvar;
            sysPOP(sysCALL("stacklength"), sysNEW_LVAR() ->> lenvar);
            pop11_comp_expr_seq_to("%")->;
            ;;; (stacklength() - lenvar) div 2
            sysCALL(sysCALL("stacklength"), sysPUSH(lenvar), "fi_-");
            sysCALL(sysPUSHQ(1), "fi_>>");
            ;;; + tmpvar -> tmpvar
            unless tmpvar then sysPOP(sysPUSHQ(0), sysNEW_LVAR() ->> tmpvar); endunless;
            sysPOP( sysCALL(sysPUSH(tmpvar), "fi_+"), tmpvar);
        else
            sysPUSHQ(XtNLookup(readitem(),"N"));    ;;; name:value
            pop11_need_nextreaditem(":")->;
            pop11_comp_expr();
            arglist_count fi_+1 -> arglist_count;
            pop11_try_nextreaditem(",")->;
        endif;
    enduntil;
    /* set arglist from stacked args */
    sysPUSH(arglist); sysPUSHQ(arglist_count);
    if tmpvar then sysCALL(sysPUSH(tmpvar), "fi_+"); endif;
    sysCALLQ(set_arg_list);
enddefine;

constant
    ;;; Common toolkit procedures get abbreviated names
    macro (
        fXptVal = [XptVal[fast]],
        CREATE = "fast_XtCreateManagedWidget",
        ADDCB = "XtAddCallback",
        DESTROY = "fast_XtDestroyWidget",
        NO_ARGS = [XptArgList([])],
      ),
;

define create_shell -> toplevelshell;
    lvars toplevelshell;
    lconstant args = writeable initXptArgList(3);
    XtAppCreateShell('poplog', 'Poplog',
        xtApplicationShellWidget,
        XptDefaultDisplay,
        XptArgs args {
            width:1,
            height:1,
            mappedWhenManaged: false,
        }) -> toplevelshell;
    XtRealizeWidget(toplevelshell);
enddefine;

global constant $-propsheet_utils = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Dec 14 1993
        Declared propsheet_utils in top section and made it global.
--- John Gibson, Apr 10 1993
        Uses xtApplicationShellWidget
--- John Gibson, Sep 11 1992
        Removed T*S_: macro
--- John Gibson, Nov  3 1991
        includes xpt_generaltypes.ph
 */
