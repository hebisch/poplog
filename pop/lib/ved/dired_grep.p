/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_grep.p
 > Purpose:        Used by ved_dired to search using 'grep' search facility
 > Author:         Aaron Sloman, Dec 18 1988 (see revisions)
 > Documentation:   HELP * DIRED, * DIRED.SHORT
 > Related Files:   LIB * VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING


uses ved_dired;

section $-dired => dired_grep;

define global dired_grep(flag, string, file, quit_first);
    ;;; Search named file(s) for occurrences of string, using
    ;;; 'grep' command

    lvars flag, string, file, quit_first;

    if sysisdirectory(file) then
        veddo('csh grep ' sys_>< string sys_>< space sys_>< file dir_>< '/*')
    else
        veddo('csh grep ' sys_>< string sys_>< space sys_>< file)
    endif;
enddefine;

"dired_grep" -> dired_action('-grep');

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Mar 23 1989 got rid of left over variables
 */
