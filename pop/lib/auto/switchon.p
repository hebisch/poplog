/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/auto/switchon.p
 >  Purpose:        provide a SWITCH/CASE syntax.
 >  Author:         Aaron Sloman, 1982 (see revisions)
 >  Documentation:  HELP * SWITCHON
 >  Related Files:
 */
compile_mode :pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/ved_declare.ph'

/*
Example:
    switchon hd(item)
        case = [] then <action1>
        notcase .islist then <action2>
        case = [=*] orcase = [=* =*] then <action3>
        case = [=?x:isword =**] andcase = [=** =?x:isword] then <action4>
        else <default action>
    endswitchon;
*/


section;


;;; Add to ved_tidy incremental lists
[case notcase orcase andcase ornotcase andnotcase
                        ^^(weakref vedbackers)] -> weakref vedbackers;

lconstant
    caseclosers     = [then or and orcase andcase ornotcase andnotcase],
    case_starters   = [case notcase endswitchon],
    ;

lvars case_stack = [];

define global vars macro endswitchon;
    if ispair(case_stack) then
        fast_back(case_stack) -> case_stack;
        "endif" :: proglist -> proglist;
    else
        mishap('msw: MISPLACED SYNTAX WORD', [endswitchon]);
    endif;
enddefine;

define lconstant case_put(x);
    ;;; plant a copy of the code that occurred between "switchon" and the first "case"
    lvars x;
    conspair(x, fast_front(case_stack) <> proglist) -> proglist;
enddefine;


define lconstant notcase_put(x);
    lvars x y;
    ;;; x will be "or" or "and";
    x, "not", "(", dl(fast_front(case_stack));
    ;;; get the rest of the condition
    readtill(caseclosers) -> y;
    ")", y;
enddefine;


define global vars macro switchon;
    lvars y;
    conspair([%readtill(case_starters) -> y%], case_stack)  -> case_stack;
        if y == "case" then
            case_put
        elseif y == "notcase" then
            notcase_put
        elseif y == "endswitchon" then
            [then endif ^^proglist] -> proglist;
            case_put
        endif("if")
enddefine;

define global vars macro case;
    case_put("elseif");
enddefine;

define global vars macro orcase;
    case_put("or");
enddefine;

define global vars macro andcase;
    case_put("and");
enddefine;

define global vars macro notcase;
    notcase_put("elseif");
enddefine;

define global vars macro ornotcase;
    notcase_put("or")
enddefine;

define global vars macro andnotcase;
    notcase_put("and")
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 11 1996
        Changed "cl*ose" to endif
--- John Gibson, Oct 17 1992
        Lconstant'ed etc
--- Simon Nichols, Aug 23 1991
        Fixed -switchon- to cope with empty body. See bugreport isl-fr.4355.
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
 */
