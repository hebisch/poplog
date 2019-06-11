/* --- Copyright University of Sussex 1988.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_chmod.p
 > Purpose:        Used by ved_dired to display or change mode of file
 > Author:         Aaron Sloman, Oct 26 1988 (see revisions)
 > Documentation:   HELP * DIRED, * DIRED.SHORT, "MAN CHMOD"
 > Related Files:   LIB * VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING


/*
Based in part on a procedure by Chris Thornton

<ENTER> dired -chmod
    Displays protection of file on current VED line.
    Display is in format for changing it

<ENTER> dired -chmod rw-r----x
<ENTER> dired -chmod 641
    Alter protection of file on current VED line.
*/

uses ved_dired;

section $-dired => dired_chmod;

define global dired_chmod(flag, file, mode, quit_first);
    ;;; Display or change access modes of current file. If mode
    ;;; is false then display access modes.
    lvars flag, file, mode, num, quit_first, modenum;

    dlocal pop_pr_radix = 8;

    define lconstant modechars(mode,i);
        ;;; return three characters corresponding to access mode
        lvars mode,i;
        if testbit(mode,i) then `r` else `-` endif;
        if testbit(mode,i-1) then `w` else `-` endif;
        if testbit(mode,i-2) then `x` else `-` endif;
    enddefine;

    define lconstant mode_num(i,string);
        ;;; i is 1, 4 or 7. String is mode-string
        lvars i,string;
        if subscrs(i,string) == `r` then 4 else 0 endif
        + if string(i+1) == `w` then 2 else 0 endif
        + if string(i+2) == `x` then 1 else 0 endif;
    enddefine;

    if mode then
        mode,file -> mode  -> file;

        if strnumber(mode) then
            strnumber('8:' sys_><mode)
        elseif isstring(mode) and datalength(mode) == 9 then
            mode_num(1,mode) << 6 + mode_num(4,mode) << 3 + mode_num(7,mode)
        else
            vederror('MODE: STRING OR INTEGER NEEDED')
        endif -> sysfilemode(file);
        unless quit_first then
            vvedlinesize + 3-> vedcolumn;
            vedinsertstring('###Access mode changed to ');
            vedinsertstring(mode);
        endunless
    else
        sys_file_stat(file, #_<{0 0 0 0 0}>_#) -> mode;
        if mode then
            fast_subscrv(5,mode) -> mode;
            cons_with consstring
            {%modechars(mode,8); modechars(mode,5); modechars(mode,2)%}
                -> mode;
            1 -> vedcolumn;
            unless quit_first or not(strmember(vedcurrentchar(),'\s\t')) then
                vedinsertstring(mode); vedinsertstring('  ');
            endunless;
            vedputcommand(vedcommand sys_>< ' ' sys_>< mode);
            vedputmessage('EDIT COMMAND AND REDO')
        else
            vederror('NO SUCH FILE')
        endif
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 28 1988
    Ouch ! unless had the wrong scope, so that vedputcommand did not
    get done when it should have. Now fixed
--- Aaron Sloman, Oct 26 1988
    Corrected documentation
 */
