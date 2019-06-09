/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptSetArg.p
 > Purpose:         Setting elements of an arglist in a clean way
 > Author:          Jonathan Meyer, Oct 10 1990 (see revisions)
 > Documentation:
 > Related Files:   XptArgList.p
 */

/*  VAL -> XptSetArg(NAME, ARGLIST, INDEX);
            XptSetArg(NAME, ARGLIST, INDEX) -> VAL;
        Sets the specified element of the ARGLIST to have the given value.
        If NAME is non-false, the NAME field is also updated.
        Using XptSetArg will ensure that the data passed to external
        procedures for a given ARGLIST remains up-to-date. ie. VAL is
        coerced into the correct representation for passing to external
        procedures.

        Example:

        lconstant arglist = initArgList(10);        ;;; make a new arglist

        100 -> XptSetArg(XtN-width,  arglist,1);    ;;; set some args.
        200 -> XptSetArg(XtN-height, arglist,2);
        ...

        lconstant width_arg = XptSetArg(%false,arglist,1%); ;;; closure
        200 -> width_arg();

        fast_XtSetValues(widget, arglist, n);
*/

section;

uses xpt_general.p

define global constant XptSetArg(name, arglist, num);
    lvars arglist, num, name, arg;
    nc_subscrXptArgList(num,arglist) -> arg -> ;    ;;; fetch arg
    arg; ;;; return value
enddefine;

define updaterof global constant XptSetArg(val, name, arglist, num);
    lvars arglist, num, name, val, arg oldname;
    nc_subscrXptArgList(num,arglist) -> -> oldname;    ;;; fetch oldname
    (name or oldname, val) -> nc_subscrXptArgList(num,arglist);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Nov  4 1991 : xt_general --> xpt_general
--- Roger Evans, Nov 19 1990 changed to use shadowclass stuff
 */
