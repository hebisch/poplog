/* --- Copyright University of Sussex 1988.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_ved.p
 > Purpose:        Used by VED_DIRED to read in a file
 > Author:         Aaron Sloman, Oct 1 1988 (see revisions)
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
dired_ved is associated with flags
    -rv -vr -wv -vw -r/ -w/ - -/
in ved_dired
*/
uses ved_dired;

section $-dired => dired_ved dired_protect_files;

define global dired_ved(flag,string1,string2,dummy);
    ;;; NB also used in dired_action table, below
    ;;; Invoke ved, possibly in protected mode, depending on whether
    ;;; flag includes `r` (read only) or `w` (writeable).
    ;;; If flag includes `/` then string2 is the file name, string1 is
    ;;; the search string to be found.
    lvars flag, string1, string2, dummy;
    dlocal dired_protect_files, vvedgotoplace;

    ;;; set up vedargument and dired_protect_files
    if strmember(`/`,flag) then
        ;;; search required
        string1 -> vvedgotoplace;
        string2 -> string1;
    endif;
    string1 -> vedargument;
    if strmember(`w`,flag) then
        false -> dired_protect_files;
    elseif strmember(`r`,flag) then
        true -> dired_protect_files
    endif;
    if strmember(`*`,vedargument)
    or strmember(`?`,vedargument)
    or sysisdirectory(vedargument)
    then
        valof("veddo")('dired ' sys_>< vedargument)
    elseif readable(vedargument) ->> flag then
        sysclose(flag);
        vededit(vedargument,
         if dired_protect_files then vedhelpdefaults else vedveddefaults endif)
    else
        vederror(NOFILE sys_>< vedargument)
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 23 1988
    Made it cope with directory or pattern
 */
