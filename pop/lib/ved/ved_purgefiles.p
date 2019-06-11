/* --- Copyright University of Birmingham 1992. All rights reserved. ------
 > File:            $poplocal/local/auto/ved_purgefiles.p
 > Purpose:         PURGE VED backup files matching pattern
 > Author:          Aaron Sloman, Sep 26 1992
 > Documentation:   HELP VED_PURGEFILES
 > Related Files:   LIB * VED_PURGEMAIL HELP * PURGE
 */


section;

global vars show_output_on_status;  ;;; used by vedpipein

define global ved_purgefiles();
    lvars arg = './', suffix = '*-', pattern, char;
    if vedargument = nullstring then
        arg sys_>< suffix
    else
        vedargument
    endif -> pattern;
    dlocal show_output_on_status = false;
    repeat
        vedputmessage(
            'PURGING FILES: ' sys_>< pattern sys_>< ' OK?(n=NO,RETURN=yes,s=show them): ');
        sys_clear_input(poprawdevin);
        vedscr_read_ascii() -> char;
        if strmember(char, 'yY\r') then
            vedputmessage('PLEASE WAIT, DELETING FILES: '
                sys_>< pattern);
            sysobey('/bin/rm ' sys_>< pattern, `%`);
            vedputmessage('DONE');
        elseif strmember(char, 'sS') then
        veddo('csh ls -C ' sys_>< pattern);
            nextloop
        else
            vedputmessage('PURGE ABORTED')
        endif;
        quitloop
    endrepeat
enddefine;

endsection;
