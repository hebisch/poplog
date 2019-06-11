/* --- Copyright University of Sussex 1988.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_purge.p
 > Purpose:        Used with VED_DIRED to purge a directory
 > Author:         Aaron Sloman, Oct 1 1988
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
associated with flag -purge
*/
uses ved_dired;

section $-dired => dired_purge;
"dired_purge" -> dired_action('-purge');

define global dired_purge(flag,file1,file2,quit_first);
    lvars flag,file1,file2, query, record, command;
    if file1 = nullstring then vederror('PURGE WHAT') endif;
    file1 sys_>< '/*-' -> file1;
    if diredgetanswer('PURGE ', file1) then
        dired_obey('rm ' sys_>< file1);
        unless quit_first then
            vedtextright(); vedinsertstring(' ### *- files deleted ###');
            vednextline();
        endunless;
        dired_setup()
    endif
enddefine;

endsection;
