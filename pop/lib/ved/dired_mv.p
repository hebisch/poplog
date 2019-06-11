/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_mv.p
 > Purpose:        Used with VED_DIRED to rename or copy a file
 > Author:         Aaron Sloman, Oct 1 1988 (see revisions)
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
dired_mv is associated with flags -mv and -cp in ved_dired
*/

include sysdefs.ph;     ;;; to work out if Berkeley Unix

section $-dired => dired_mv;

define global dired_mv(flag, file1, file2, quit_first);
    ;;; Rename file2 as file1
    lvars flag, file1, file2, name2, quit_first, query, record, command;
    if not(file2) or file2 = nullstring then
        vederror('MOVE or COPY TO WHAT')
    endif;
    if flag = '-mv' then  'MOVE ',  sys_file_move,  ' ###MOVED--> '

    elseif flag = '-cp' then 'COPY ',   sys_file_copy,  ' ###COPIED--> '

    endif -> record -> command -> query;

    sys_fname_name(file2) -> name2;
    if sysisdirectory(file1) then
        cons_with consstring {%
            explode(query), explode('(to directory '),
            explode(file1), explode(' ) ') %} -> query;
        file1 dir_>< name2 -> file1;
    endif;
    if diredgetanswer(query, name2) then
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

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun  3 1990
    Made it print only file name+extension, not whole path name, when
    checking.
--- Aaron Sloman, Apr 16 1990
    Altered so that if target filename names a directory then it moves
    the file to that directory with the same name.
--- Aaron Sloman, Aug 13 1989
    Because sys_file_move can now handle directories on Berkeley Unix, it
    is no longer necessary to call sysobey.
--- Aaron Sloman, Nov 27 1988
    Fixed to handle case of renaming a directory
--- Aaron Sloman, Oct 17 1988
    made to use -sys_file_move- and -sys_file_copy- and fixed error check
 */
