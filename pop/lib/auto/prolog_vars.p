/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/auto/prolog_vars.p
 > Purpose:        Macro for making and assigning new prologvars
 > Author:         Kathryn Seifert, Aug 26 1986
 > Documentation:  HELP * PROLOG_VARS, TEACH * PROLOGINPOP
 > Related Files:  LIB * PROLOG_LVARS
 */

section;

define global macro prolog_vars;
    lvars list = [], item;
    until (readitem() ->> item) == ";" do
        unless isword(item) do
            mishap(item, 1, 'Impermissible item in prolog_vars statement');
        elseunless item == ";" or item == "," do
            item :: list -> list;
        endunless;
    enduntil;
    for item in list do
        if isprotected(item) then
            mishap(item, 1, 'Cannot prolog_vars a system identifier');
        else
            sysVARS(item, 0);
            sysCALLQ(prolog_newvar);
            sysPOP(item);
        endif;
    endfor;
enddefine;

endsection;
