/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCheckVarargs.p
 > Purpose:         Check for varargs list on stack
 > Author:          John Gibson, May  2 1993
 > Documentation:   REF *XPT_TYPECHECK
 */
compile_mode :pop11 +strict;

section;

include xt_constants;

;;; Check the top N items on the stack form a variable length argument list
;;; - 15/08/90
define XptCheckVarargs(n);
    lvars n, current_item;
    unless isinteger(n) then
        mishap(n, 1, 'Varargs COUNT NEEDED');
    else
        while n fi_> 0 do;
            XptCheckString(subscr_stack(n)) -> current_item;
            if current_item = XtVaNested
            or datalength(current_item) == #_< datalength(XtVaNested) >_#
            and issubstring_lim(current_item,1,1,false,XtVaNested)
            then
                if n fi_> 1 then
                    ;;; XptCheckVarargsList
                    XptLiveTypeCheck(subscr_stack(n fi_- 1),"VarArgsList") ->;
                endif;
                n fi_- 2 -> n;
            elseif current_item = XtVaTypedArg
            or datalength(current_item) == #_< datalength(XtVaTypedArg) >_#
            and issubstring_lim(current_item,1,1,false,XtVaTypedArg)
            then
                if n fi_> 4 then
                    XptCheckString(subscr_stack(n fi_- 1)) ->;
                    XptCheckString(subscr_stack(n fi_- 2)) ->;
                    XptCheckInt(subscr_stack(n fi_- 4)) ->;
                endif;
                n fi_- 5 -> n;
            else
                n fi_- 2 -> n;
            endif;
        endwhile;
        if n fi_< 0 then
            mishap(current_item, 1, 'MISSING Vararg ARGUMENT(S)');
        endif;
    endunless;
enddefine;

endsection;
