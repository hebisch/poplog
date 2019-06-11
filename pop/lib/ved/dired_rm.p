/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_rm.p
 > Purpose:        Used by ved_dired to delete a file
 > Author:         Aaron Sloman, Oct 1 1988 (see revisions)
 > Documentation:   HELP * DIRED, * DIRED.SHORT
 > Related Files:   LIB * VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
dired_rm is associated with flags -rm, -rm-r and -rmdir in ved_dired
*/

uses ved_dired;

section $-dired => dired_rm;

define global dired_rm(flag, file, dummy, quit_first);
    ;;; Delete file, after checking. Unless quit_first is true put
    ;;; information about deletion at end of current line.

    lvars flag, file, dummy, quit_first,
         rflag = 'rm -r ', dirflag = 'rmdir ', oldmishap=prmishap;


    if flag = '-rm-r' then rflag -> flag
    elseif flag = '-rmdir' then dirflag -> flag
    endif;
    unless readable(file) then vederror('NO SUCH FILE') endunless;
    if flag == rflag or flag == dirflag then goto DIR
    elseif diredgetanswer('DELETE ', file) then
        define dlocal prmishap(string,list);
            lvars string, list;
            dlocal prmishap=oldmishap;
            exitfrom(false,sysunlink)
        enddefine;

        if (sysunlink(file,false), oldmishap -> prmishap) then
        elseif sysisdirectory(file) then
DIR:
            if diredgetanswer('DELETE (directory) ', file) then
                if flag = rflag then
                    dired_obey(flag sys_>< file)
                else
                    dired_obey(dirflag sys_>< file);
                endif
            else return
            endif;
            if readable (file) ->> file then
                ;;; failed to delete it
                sysclose(file);
                goto ERR
            endif;
        else
ERR:
            vederror('CANNOT DELETE: ' sys_>< sysiomessage())
        endif;
        unless quit_first then
            vedtextright();
            vedinsertstring(' ###DELETED###');
            vednextline();
        endunless;
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  3 1995
        Changed vars to dlocal
--- Aaron Sloman, Oct 17 1988
    Altered to cope with symbolic links to directories. UGH
 */
