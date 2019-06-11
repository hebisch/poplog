/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_mvd.p
 > Purpose:        Used with VED_DIRED to rename a file, in same directory
 > Author:         Aaron Sloman, 16 April 1990
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED *DIRED_MV *DIRED_CPD
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
dired_mvd is associated with flag -mvd

  <ENTER> -mvd <name>
    Changes the name of the file on current VED line to be <name> but
    keeps it in the same directory.

  <ENTER> -cpd <name>
    Copies the file named on current VED line to file called <name> but
    in the same directory.
*/

include sysdefs.ph;     ;;; to work out if Berkeley Unix

section $-dired => dired_mvd;

define global dired_mvd(flag,file1,file2,quit_first);
    ;;; Rename file2 as file1
    lvars flag,file1,file2,quit_first,query, record, command;
    if not(file2) or file2 = nullstring then
        vederror('MOVE or COPY TO WHAT')
    endif;

    if flag = '-mvd' then  'MOVE ',     sys_file_move,  ' ###MOVED--> '

    elseif flag = '-cpd' then 'COPY ',  sys_file_copy,  ' ###COPIED--> '

    endif -> record -> command -> query;

    if diredgetanswer(query, file2) then
        ;;; work out new path name from old directory and new name
        sys_fname_path(file2) dir_>< file1 -> file1;
#_IF DEF BERKELEY
        ;;; no longer need to use sysobey to rename a directory
        command(file2, file1);
#_ELSE
        if command == sys_file_move and sysisdirectory(file2) then
            sysobey('mv ' sys_>< file2 sys_>< space sys_>< file1)
        else
            command(file2, file1);
        endif;
#_ENDIF
        unless quit_first then
            vedtextright(); vedinsertstring(record); vedinsertstring(file1);
            vednextline();
        endunless;
        dired_setup()
    endif
enddefine;

"dired_mvd" ->> dired_action('-mvd') -> dired_action('-cpd');

endsection;
