/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/auto/prolog_lvars.p
 > Purpose:        Macro for making and assigning new prologvars
 > Author:         Kathryn Seifert, Aug 26 1986 (see revisions)
 > Documentation:  HELP * PROLOG_VARS, TEACH * PROLOGINPOP
 > Related Files:  LIB * PROLOG_VARS
 */

section;

define global macro prolog_lvars;
    lvars list = [], item;
    until (readitem() ->> item) == ";" do
        unless isword(item) do
            mishap(item, 1, 'Impermissible item in prolog_lvars statement');
        elseunless item == ";" or item == "," do
            conspair(item,list) -> list;
        endunless;
    enduntil;
    for item in list do
        if isprotected(item) then
            mishap(item, 1, 'Cannot prolog_lvars a system identifier');
        else
            sysLVARS(item, 0);
            sysCALLQ(prolog_newvar);
            sysPOP(item);
        endif;
    endfor;
enddefine;

endsection;
