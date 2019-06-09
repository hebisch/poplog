/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptEvenlySizeWidgets.p
 > Purpose:         Make a number of widgets the same width/height
 > Author:          Jonathan Meyer, Aug 17 1991 (see revisions)
 > Documentation:   REF *XT_LIBS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define lconstant process_spec(n, spec);
    lvars n, spec, i,  w, width, height, new_width=0, new_height=0,
            procedure (test_width_p, test_height_p);

    /* deal with the "width" spec */
    spec(1) -> i;
    if i == false or i.isinteger then
        false -> new_width;
    elseif i == "min" then
        fi_min -> test_width_p;
        2**29-1 -> new_width;
    elseif i == "max" then
        fi_max -> test_width_p;
    else
        mishap(i,1,'INVALID WIDTH SPECIFICATION');
    endif;

    /* deal with the "height" spec */
    spec(2) -> i;
    if i == false or i.isinteger then
        false -> new_height;
    elseif i == "min" then
        fi_min -> test_height_p;
        2**29-1 -> new_height;
    elseif i == "max" then
        fi_max -> test_height_p;
    else
        mishap(i,1,'INVALID HEIGHT SPECIFICATION');
    endif;

    fast_for i from 1 to n do
        XptWidgetCoords(subscr_stack(i)) -> (,, width, height);
        if new_width then test_width_p(width, new_width) -> new_width endif;
        if new_height then test_height_p(height, new_height) -> new_height endif;
    endfast_for;

    new_width or spec(1), new_height or spec(2);
enddefine;

lvars arglist = false;

define global XptEvenlySizeWidgets(n, spec);
    lvars n, spec, argcount = 1, x, y;

    /* use an arglist since we may be setting lots of widgets values */
    unless arglist then initXptArgList(2) -> arglist endunless;

    process_spec(n, spec) -> (x,y);

    if x then
        XtN width, x -> nc_subscrXptArgList(argcount, arglist);
        argcount fi_+1 -> argcount;
    endif;

    if y then
        XtN height, y -> nc_subscrXptArgList(argcount, arglist);
        argcount fi_+1 -> argcount;
    endif;

    fast_repeat n times
        fast_XtSetValues(arglist, argcount);
    endfast_repeat;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 22 1993
        Made XptArgList structure be created at run-time and cached (POPC
        can't handle compile-time creation of shadowclass instances).
 */
