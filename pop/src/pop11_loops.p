/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/pop11_loops.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYNTAX, REF *POPSYNTAX
 */

;;; ---------------- COMPILING LOOP CONSTRUCTS --------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'

constant
        procedure (subscrl, isnumber, destlist, sys_current_ident,
        sys_read_path,
        pop11_comp_expr, pop11_comp_expr_to, pop11_comp_stmnt_seq_to,
        pop11_need_nextitem, pop11_try_nextitem, is_sub_syntax_word,
        Sys$-Check_word, Sys$-Pop11$-Test_missing_sep,
        )
    ;

vars
        pop_new_lvar_list, pop_syntax_only
    ;


;;; --- DECLARATIONS OF NON-OPENER SYNTAX WORDS -------------------------

constant
    Sys$-Pop11$-Loops   = true; ;;; used to force inclusion of this file

constant syntax (
    elseif          = pop_undef,
    else            = pop_undef,
    endif           = pop_undef,
    then            = pop_undef,
    elseunless      = pop_undef,
    endunless       = pop_undef,
    endwhile        = pop_undef,
    enduntil        = pop_undef,
    do              = pop_undef,
    step            = pop_undef,
    till            = pop_undef,
    endfor          = pop_undef,
    endfast_for     = pop_undef,
    endrepeat       = pop_undef,
    endfast_repeat  = pop_undef,
    times           = pop_undef,
    forever         = pop_undef,
    on              = pop_undef,
    in              = pop_undef,
    on_list         = pop_undef,
    in_list         = pop_undef,
    by              = pop_undef,
    from            = pop_undef,
    to              = pop_undef,
    using_subscriptor= pop_undef,
);

vars
    Sys$-Pop11$-loop_starts = [],
    Sys$-Pop11$-loop_ends   = [],
    ;


;;; --- QUITLOOP AND NEXTLOOP --------------------------------------------

define pop11_loop_start(looplabel);
    lvars looplabel;
    looplabel :: Sys$-Pop11$-loop_starts -> Sys$-Pop11$-loop_starts
enddefine;

define pop11_loop_end(loopendlabel);
    lvars loopendlabel;
    loopendlabel :: Sys$-Pop11$-loop_ends -> Sys$-Pop11$-loop_ends
enddefine;

define lconstant Loop_control(lablist_id, jump_pdr);
    lvars lablist_id, level, list, procedure jump_pdr;
    lconstant   inv_ms = '%INVALID LOOP NUMBER FOR %S',
                inv_idstring = 'pop11-loopnum:syntax';
    if jump_pdr /== sysGOTO then
        ;;; compile a condition
        pop11_need_nextitem("(") -> ;
        pop11_comp_expr_to(")") ->
    endif;
    if pop11_try_nextitem("(") then
        itemread() -> level;
        if level == ")" then
            1 -> level
        elseunless isinteger(level) and level fi_> 0 then
            mishap(pdprops(caller(1)), level, 2, inv_ms, inv_idstring)
        else
            pop11_need_nextitem(")") ->
        endif
    else
        1 -> level
    endif;
    returnif(pop_syntax_only);

    fast_idval(lablist_id) -> list;
    if level fi_> listlength(list) then
        mishap(pdprops(caller(1)), level, 2, inv_ms, inv_idstring)
    else
        ;;; compile a (conditional) jump to the selected label
        jump_pdr(list(level))
    endif
enddefine;

define syntax nextloop;
    Loop_control(ident Sys$-Pop11$-loop_starts, sysGOTO)
enddefine;

define syntax nextif;
    Loop_control(ident Sys$-Pop11$-loop_starts, sysIFSO)
enddefine;

define syntax nextunless;
    Loop_control(ident Sys$-Pop11$-loop_starts, sysIFNOT)
enddefine;

define syntax quitloop;
    Loop_control(ident Sys$-Pop11$-loop_ends, sysGOTO)
enddefine;

define syntax quitif;
    Loop_control(ident Sys$-Pop11$-loop_ends, sysIFSO)
enddefine;

define syntax quitunless;
    Loop_control(ident Sys$-Pop11$-loop_ends, sysIFNOT)
enddefine;


;;; --- WHILE, UNTIL ------------------------------------------------------

define lconstant Iteration_compile(popclosebracket, testpdr);
    lvars endlabel, label, procedure testpdr;
    dlocal popclosebracket;
    sysNEW_LABEL() -> endlabel;
    sysNEW_LABEL() -> label;
    sysLABEL(label);
    pop11_comp_expr_to([do then]) -> ;
    testpdr(endlabel);
    pop11_loop_end(endlabel);
    pop11_loop_start(label);
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;
    sysGOTO(label);
    sysLABEL(endlabel)
enddefine;

define syntax while;
    Iteration_compile([endwhile {close}], sysIFNOT)
enddefine;

define syntax until;
    Iteration_compile([enduntil {close}], sysIFSO)
enddefine;


;;; --- FOR -------------------------------------------------------------

lconstant
    one_ms      = 'for: ONLY ONE LOOP VARIABLE ALLOWED',
    one_idstring= 'pop11-for-vars1:syntax'
    ;

define lconstant list_iteration(varlist, in_or_on, isfast, using_subscr);
    lvars   varlist, in_or_on, isfast, using_subscr,
            endlabel, looplabel, label,
            countvar, lengthvar, subscrvar,
            listvars, var, tx_word
        ;
    pop11_loop_end(sysNEW_LABEL() ->> endlabel);        ;;; end label
    pop11_loop_start(sysNEW_LABEL() ->> looplabel);     ;;; looping label
    sysNEW_LABEL() -> label;                    ;;; label to br round initial
    if in_or_on then
        [% for var in varlist do sysNEW_LVAR() endfor %] -> listvars;
        pop11_comp_stmnt_seq_to([do then using_subscriptor]) -> tx_word;
        if tx_word == "using_subscriptor" and length(varlist) /== 1 then
            mishap(destlist(varlist), one_ms, one_idstring)
        endif;
        applist(listvars, sysPOP);
        if tx_word == "using_subscriptor" then
            unless using_subscr then
                mishap(0, 'for: INVALID using_subscriptor WITH in_list/on_list',
                                        'pop11-for-usinv:syntax')
            endunless;
            hd(listvars) -> var;
            sysNEW_LVAR() -> countvar;
            sysNEW_LVAR() -> lengthvar;
            sysNEW_LVAR() -> subscrvar;
            sysPUSHQ(1), sysPOP(countvar);
            sysCALL(sysPUSH(var), "length"), sysPOP(lengthvar);
            pop11_comp_expr_to([do then])->, sysPOP(subscrvar);
            sysGOTO(label);
            sysLABEL(looplabel);
            sysCALL(sysPUSH(countvar), sysPUSHQ(1), "fi_+"), sysPOP(countvar);
            sysLABEL(label);
            sysCALL(sysPUSH(countvar), sysPUSH(lengthvar), "fi_>");
            sysIFSO(endlabel);
            sysCALL(sysPUSH(countvar), sysPUSH(var), subscrvar);
            sysPOP(front(varlist))
        else
            rev(listvars) -> listvars;
            sysLABEL(looplabel);
            until varlist == [] do
                sysPUSH(front(listvars));
                if isfast then
                    sysCALL(sysPUSH("nil"), "==")
                else
                    sysCALL("null")
                endif;
                sysIFSO(endlabel);
                sysCALL(sysPUSH(front(listvars)), "fast_destpair");
                sysPOP(front(listvars)), sysPOP(front(varlist));
                fast_back(listvars) -> listvars;
                fast_back(varlist) -> varlist
            enduntil
        endif
    else ;;; "on"
        pop11_comp_stmnt_seq_to([do then]) -> ;
        rev(varlist) -> varlist;
        applist(varlist, sysPOP);
        sysGOTO(label);
        sysLABEL(looplabel);
        for var in varlist do
            sysCALL(sysPUSH(var), if isfast then "fast_back" else "tl" endif);
            sysPOP(var)
        endfor;
        sysLABEL(label);
        for var in varlist do
            sysPUSH(var);
            if isfast then
                sysCALL(sysPUSH("nil"), "==")
            else
                sysCALL("null")
            endif;
            sysIFSO(endlabel);
        endfor;
    endif;
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;
    sysGOTO(looplabel);
    sysLABEL(endlabel)
enddefine;

define lconstant Get_varexpr(terminators, var) -> terminator -> item;
    lvars item terminators terminator var;
    ;;; var is either false or a variable. If it is a variable then POP the
    ;;; expression into it here else ...
    ;;; ... item is either a value or an identifier containing it.
    itemread() -> item;
    if pop11_try_nextitem(terminators) ->> terminator then
        unless isnumber(item) then Sys$-Check_word(item) endunless;
        if var then
            if isnumber(item) then sysPUSHQ else sysPUSH endif(item);
            sysPOP(var);
            var -> item;
        endif;
    else
        item :: proglist -> proglist;
        if var then var else sysNEW_LVAR() endif -> item;
        pop11_comp_expr_to(terminators) -> terminator, sysPOP(item)
    endif
enddefine;

define lconstant numeric_iteration(varlist, frombyto, isfast);
    lvars   var, varlist, frombyto, isfast,
            fromval, byval, toval, term, by_or_to,
            start, fin, testlabel, nxtloop,
            going_down, lt_p, gt_p
        ;
    if length(varlist) > 1 then
        mishap(destlist(varlist), one_ms, one_idstring)
    endif;
    front(varlist) -> var;

    pop11_loop_end(sysNEW_LABEL() ->> fin);
    sysNEW_LABEL() -> start;
    pop11_loop_start(sysNEW_LABEL() ->> nxtloop);
    sysNEW_LABEL() -> testlabel;      ;;; label to br to first test

    ;;; get initial from value
    if frombyto == "from" then
        Get_varexpr([by to], var) -> by_or_to -> fromval;
    else
        frombyto -> by_or_to; 1 -> fromval;
        sysPUSHQ(1); sysPOP(var);
    endif,

    ;;; get step(by) value
    if by_or_to == "to" then
        1 -> byval
    else
        Get_varexpr("to", false) -> -> byval;
    endif;

    ;;; get end value and jump to test
    Get_varexpr([do then], false) -> -> toval;

    if isinteger(fromval) and isinteger(byval) and isinteger(toval) then
        ;;; optimise to be a fast loop
        true -> isfast;
    endif;

    sysGOTO(testlabel);

    ;;; body of loop
    sysLABEL(start);
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;
    sysLABEL(nxtloop);

    define lconstant op2(arg1, arg2, pname);
        lvars arg1, arg2, pname;
        define lconstant push();
            chain(if isword(dup()) then sysPUSH else sysPUSHQ endif)
        enddefine;
        sysCALL(push(arg1), push(arg2), pname)
    enddefine;

    ;;; increment user variable (we jump over this first time round)
    op2(var, byval, if isfast then "fi_+" else "+" endif);
    sysPOP(var);

    ;;; continue loop if test ok
    sysLABEL(testlabel);
    if isfast then "fi_>", "fi_<" else ">", "<" endif -> lt_p -> gt_p;
    if isword(byval) then
        ;;; plant run-time direction-checking test
        op2(byval, 0, gt_p);
        sysIFNOT(sysNEW_LABEL() ->> going_down);
        op2(var, toval, gt_p);
        sysIFNOT(start);
        sysGOTO(fin);
        sysLABEL(going_down);
        op2(var, toval, lt_p)
    else
        ;;; we already know which way the count will be going
        op2(var, toval, if byval > 0 then gt_p else lt_p endif)
    endif;
    sysIFNOT(start);

    sysLABEL(fin)
enddefine;

define lconstant For_procedure(isfast);
    ;;; closures of this used for "for" and "fast_for"
    lvars   item, varlist, endlabel, looplabel, label,
            isfast, proc, progl = proglist, id
        ;
    dlocal pop_new_lvar_list, popclosebracket = [endfor endfast_for {close}];
    lconstant
        none_ms         = 'for: MISSING ITERATION VARIABLE(S)',
        none_idstring   = 'pop11-for-vars0:syntax';

    [%  repeat
            itemread() -> item;
            if isword(item) and (sys_current_ident(item) ->> id)
            and id!ID_IDENTPROPS _bitst _:M_ID_SYNTAX and item /== "$-" then
                quitunless(item == ",")
            else
                sys_read_path(item, false, false)
            endif
        endrepeat
     %] -> varlist;

    if item == "in" or item == "on" then
        if varlist == [] then mishap(0, none_ms, none_idstring) endif;
        list_iteration(varlist, item == "in", isfast, true)
    elseif item == "in_list" or item == "on_list" then
        if varlist == [] then mishap(0, none_ms, none_idstring) endif;
        list_iteration(varlist, item == "in_list", isfast, false)
    elseif item == "from" or item == "by" or item == "to" then
        if varlist == [] then mishap(0, none_ms, none_idstring) endif;
        numeric_iteration(varlist, item, isfast);
    elseif is_sub_syntax_word(item, "for") ->> proc then
        ;;; we've found an iteration extension handler
        ;;; need to call instead of chaining because of popclosebracket
        if varlist == [] then mishap(0, none_ms, none_idstring) endif;
        proc(varlist, isfast)
    else
        ;;; none of the above so it must be ...
        ;;; for <action> step <action> till <condition> do <actions> endfor
        ;;; ... so reset proglist and start from scratch
        progl -> proglist;
        pop11_loop_end(  sysNEW_LABEL() ->> endlabel);      ;;; end label
        pop11_loop_start(sysNEW_LABEL() ->> looplabel);     ;;; looping label
        sysNEW_LABEL() -> label;                    ;;; label to br round initial

        pop11_comp_stmnt_seq_to("step") -> ;
        sysGOTO(label);
        sysLABEL(looplabel);
        pop11_comp_stmnt_seq_to("till") -> ;
        sysLABEL(label);
        pop11_comp_expr_to([do then]) -> ;
        sysIFSO(endlabel);
        pop11_comp_stmnt_seq_to(popclosebracket) -> ;
        sysGOTO(looplabel);
        sysLABEL(endlabel)
    endif
enddefine;

define global syntax for;
    For_procedure(false)
enddefine;

define global syntax fast_for;
    For_procedure(true)
enddefine;


;;; --- REPEAT -----------------------------------------------------------

define lconstant Repeat(isfast);
    lvars start, fin, countvar, isfast, item;
    dlocal pop_new_lvar_list,
        popclosebracket = [endrepeat endfast_repeat {close}];
    sysNEW_LABEL() -> start;
    sysNEW_LABEL() -> fin;
    pop11_loop_end(fin);
    pop11_loop_start(start);
    sysLABEL(start);            ;;; this is redundant for "times"
    unless pop11_try_nextitem("forever") then
        itemread() -> item;
        if isinteger(item) and nextitem() == "times" then
            sysPUSHQ(item);
            ;;; can optimise integer count
            true -> isfast
        else
            item :: proglist -> proglist;
            pop11_comp_expr()
        endif;
        if pop11_try_nextitem("times")  then
            ;;; doing a counted loop so pop the count ...
            sysPOP(sysNEW_LVAR() ->> countvar);
            ;;; ... and replace the start label with a new one
            sysNEW_LABEL() ->> start -> hd(Sys$-Pop11$-loop_starts);
            sysLABEL(start);
            sysCALL(sysPUSH(countvar), sysPUSHQ(0),
                                    if isfast then "fi_>" else ">" endif);
            sysIFNOT(fin);
            sysCALL(sysPUSH(countvar), sysPUSHQ(1),
                                    if isfast then "fi_-" else "-" endif);
            sysPOP(countvar)
        else
            Sys$-Pop11$-Test_missing_sep(nextitem())
        endif;
    endunless;
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;
    sysGOTO(start);
    sysLABEL(fin)
enddefine;

define syntax repeat;
    Repeat(false)
enddefine;

define syntax fast_repeat;
    Repeat(true)
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 11 1996
        Made Loop_control not check in loop starts/ends when pop_syntax_only.
        Added mishap idstrings.
--- John Gibson, Apr 11 1996
        "close" now in a vector for closing brackets (only recognised if
        declared syntax).
--- John Gibson, Oct 31 1992
        Moved in relevant non-opener syntax word initialisations from
        pop11_syntax.p
--- John Gibson, May 15 1989
        Included ident.ph
--- Ian Rogers, May 11 1989
        -in_list- used to do the same as -on_list- and -on- (ie. successive
        tails. Now works correctly.
--- John Gibson, Apr 18 1989
        Rewrote code to read iteration variables in -For_procedure- (now
        handles lexical identifiers and section pathnames correctly).
--- John Gibson, Apr 16 1989
        Changed test for -for- extension to use -is_sub_syntax_word-.
--- Ian Rogers, Apr 14 1989
        Corrected my mistakes
--- Ian Rogers, Apr 12 1989
        Moved -pop11_forloop_test- to be an autoloadable as it is no longer
        needed here.
--- Ian Rogers, Apr 11 1989
        Added new *FOR_FORM stuff, tidied For_procedure, optimised numeric
        "for" when all clauses are integers. Tidied and optimised repeat.
--- Aaron Sloman, Apr 29 1988
        Improved the error message
--- Aaron Sloman, Apr 29 1988 stop
        Inserted check for missing loop variable
--- John Gibson, Mar 20 1988
        Added -fast_repeat-
 */
