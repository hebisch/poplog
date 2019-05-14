/* --- Copyright University of Birmingham 1995. All rights reserved. ------
 > File:            $poplocal/local/auto/while_matching.p
 > Purpose:         A looping construct based on doesmatch
 > Author:          Aaron Sloman, Nov 26 1995
 > Documentation:   HELP * DOESMATCH
 > Related Files:   LIB * READPATTERN, LIB * DOESMATCH
 */

/*

A new looping construct based on fmatching in LIB * FMATCHES:

     while_matching <list> with <pattern> do <action> endwhile_matching
     while_matching <list> with <pattern> when <condition> do <action> endwhile_matching

Example:

lvars x, y;

while_matching [1 2 3 4] with ![== ??x = ??y == ] do
    if x /== [] and y /== [] then
        [^x ^y] =>
    endif;
endwhile_matching;

;;; Now using a when clause instead of if then
lvars x,y;
while_matching [1 2 3 4] with ![== ??x = ??y == ] when x /== [] and y /== [] do
    [^x ^y] =>
endwhile_matching;

;;; Now without
lvars x,y;
while_matching [1 2 3 4] with ![== ??x = ??y == ] do
    [^x ^y] =>
endwhile_matching;



*/

compile_mode: pop11 +strict;

uses readpattern;

section;

applist([with when while_matching endwhile_matching], sysunprotect);

global constant syntax
    (with = pop_undef, when = pop_undef, endwhile_matching= pop_undef);

define global syntax while_matching;
    lvars item, label;
    ;;; read in the datum
    pop11_comp_expr_to("with") ->;

    ;;; Now read in the pattern, replacing pattern variables with
    ;;; identifiers. But first check that it is a list.
    if nextitem() == "!" then
        readitem() ->;
        readpattern(false); ;;; repace variables with identifier
                            ;;; but don't allow "where"
    elseif nextitem() = "[" then
        pop11_comp_expr();  ;;; compile a list expression
    else
        mishap('EXPECTING LIST AFTER "with", FOUND ' sys_>< nextitem())
    endif;

    ;;; check that "when" or "do" follows
    pop11_need_nextreaditem([do when]) -> item;

    sysPROCEDURE("for_matching",0);
    sysNEW_LABEL() -> label;    ;;; may be redundant

    if item == "when" then
        ;;; compile -when- clause
        pop11_comp_expr_to("do") ->;
        sysIFNOT(label);
    endif;

    ;;; now compile the procedure body after "do"
    pop11_comp_stmnt_seq_to([endwhile endwhile_matching]) ->;
    sysLABEL(label);
    sysPUSHQ(false);    ;;; procedure always returns false, to force repeat
    sysPUSHQ(sysENDPROCEDURE());
    sysCALL("doesmatch");
    sysERASE(0);
enddefine;


#_INCLUDE '$usepop/pop/lib/include/ved_declare.ph'

[^^(weakref vedopeners) while_matching] -> weakref vedopeners;
[^^(weakref vedbackers) when with] -> weakref vedbackers;
[^^(weakref vedclosers) endwhile_matching] -> weakref vedclosers;


applist([with when while_matching endwhile_matching], sysprotect);
endsection;
