/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/src/xt_call.p
 > Purpose:         Interface for calling X toolkit routines
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt =>
                        XptCoerceString,
                        XptCoerceTmpString,
                        XptCoerceBoolean,
                        XptCoerceVarargs,
                        XptCoerceXtPointer,
                        ;

/* -- Coercion Routines --------------------------------------- */

/*  These routines are used for coercion of arguments above and beyond what
    -call_external- already does */


define XTC_str(x);
    lvars x;
    if x then
        if  x!V_BYTES[x!V_LENGTH _sub _1] /== _0 then
            cons_fixed(#| explode(x),0 |#, string_key)
        else
            x;
        endif;
    else
        0;  ;;; convert <false> to (pop) NULL
    endif;
enddefine;

define XptCoerceString(x);
    lvars x;
    if x == 0 then
        false
    else
        Check_string(x);
        if  x!V_BYTES[x!V_LENGTH _sub _1] /== _0 then x
        else substring(1,datalength(x) fi_- 1, x);
        endif;
    endif;
enddefine;

define updaterof XptCoerceString(x);
    lvars x;
    if x then Check_string(x) endif;
    XTC_str(x);
enddefine;

define XptCoerceTmpString with_nargs 1;
    fast_chain(XptCoerceString);
enddefine;

define updaterof XptCoerceTmpString(x);
    lvars x;
    ;;; convert <false> to (pop) NULL
    if x then Check_string(x), x else 0 endif;
enddefine;


define XTC_bool(b);
    lvars b;
    if b then XtTrue else XtFalse endif;
enddefine;

define XptCoerceBoolean with_nargs 1;
    not(() == 0);
enddefine;

define updaterof XptCoerceBoolean with_nargs 1;
    fast_chain(XTC_bool);
enddefine;

define XTC_generic(item);
    lvars item;
    if isstring(item) then XTC_str(item)
    elseif isboolean(item) then XTC_bool(item)
    else item;
    endif;
enddefine;

;;; no coercion possible - do nothing
define XptCoerceXtPointer with_nargs 1;
enddefine;

define updaterof XptCoerceXtPointer(x);
    lvars x;
    XTC_generic(x);
enddefine;

;;; no coercion possible -- just fiddle the arg counts
define XptCoerceVarargs(n);
    lvars n;
    ->;             ;;; Remove top zero from stack ...
    n fi_- 1    ;   ;;; ... and decrement count to compensate
enddefine;
;;;
define updaterof XptCoerceVarargs(n);
    lvars n, pos;
    fast_for pos from 1 to n do
        XTC_generic(subscr_stack(pos)) -> subscr_stack(pos)
    endfor;
    0;          ;;; null terminate the varargs list
    n fi_+ 1;   ;;; stack new varargs count
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  5 1996
        Removed use of silly procedure St*ack_apply in XptCoerceVarargs
        updater
--- John Gibson, Mar 30 1995
        Removed XptEx*accPopObj (defunct for some time)
--- John Gibson, Apr 13 1994
        Got rid of deferred actions stuff (XptDeferApply now autoloadable)
--- John Gibson, Feb 28 1994
        Rewrote Apply_def*erred_actions to fix bugs. (Was doing a CHECKINTERRUPT
        in between testing def*erred_actions empty and getting the next
        element; also, the use of null could cause problems).
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- John Gibson, Sep  6 1992
        Arg conversion previously done by X*TC_varargs is no longer
        necessary -- so made it be the updater of XptCoerceVarargs (which
        probably isn't necessary either).
--- John Gibson, Aug 19 1992
        Made deferred actions be applied with sys_raise_signal so they can be
        blocked like other signals/asts (also changed names of internal var
        and procedure so they don't look like exported things).
--- John Gibson, Sep 14 1991
        Made updater of XptCoerceTmpString just return x if x a string, since
        all strings are now null-terminated. XTC_str_temp therefore
        no longer required.
--- Adrian Howard, Jul  9 1991 : Corrected XptCoerceBoolean, use to check that
        value == 1, now checks that /== 0
--- Roger Evans, Jul  1 1991 added deferred actions code
--- Roger Evans, Feb  6 1991 got rid of some silly attempted coercions of
        untyped data
--- Roger Evans, Nov 20 1990 changed 'datalist' to 'explode'
--- Roger Evans, Nov 18 1990
    undid previous three changes and added XptEx*accPopObj instead
--- Roger Evans, Nov 17 1990
    changed XTC_generic to handled fixed address structs properly
--- Roger Evans, Nov 16 1990 changed XTC_str etc to return real _NULL
--- Roger Evans, Nov 15 1990 made the coerced strings fixed
--- Roger Evans, Oct 20 1990 added varargs and XtPointer
--- Roger Evans, Oct 19 1990 added public coercion routines
--- Roger Evans, Jul  4 1990 added X_apply
--- Roger Evans, May  4 1990 EXP_ADDRESS -> XP_PTR
 */
