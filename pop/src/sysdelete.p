/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/src/sysdelete.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSUTIL
 */

;;; ----------------- DELETE A FILE (UNIX) ------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'unixdefs.ph'

global constant
        procedure (syslink, sysunlink)
    ;

global vars
        pop_file_versions
    ;

section $-Sys$-Io;

constant
        procedure (Symlink_target, File_nlinks, File_copy)
    ;

endsection;

;;; ------------------------------------------------------------------

section $-Sys$-Io => sysdelete;

define sysdelete(file);
    lvars file, follow_symlinks, oldfile, callr, _nlinks;
    dlocal pop_file_versions;
    if isboolean(file) then
        file -> follow_symlinks;
        -> file
    else
        true -> follow_symlinks
    endif;
    Symlink_target(file, follow_symlinks) -> file;
    File_nlinks(file, false) -> (_nlinks, );
    ;;; just do sysunlink if not regular file or no old versions required
    returnunless(_nlinks /== true
                 and isinteger(pop_file_versions) and pop_file_versions fi_> 1)
                    (sysunlink(file, false));
    ;;; return false if doesn't exist
    returnif(_nlinks == 0) (false);
    caller(1) -> callr;
    unless _nlinks then
        if callr /== sysdelete then
            Syserr_mishap(file, 1, 'CAN\'T DELETE FILE')
        else
            0 -> _nlinks
        endif
    endunless;
    if _nlinks fi_<= 1 or callr == sysdelete then
        ;;; not accessible / doesn't have links / old version
        sysunlink(file, false) -> ;     ;;; error if not accessible
        sysfileok(file sys_>< '-') -> oldfile;
        if syslink(oldfile, file, false) then
            pop_file_versions fi_- 1 -> pop_file_versions;
            sysdelete(oldfile) ->
        endif
    else
        ;;; file has links
        sysfileok(file sys_>< '-') -> oldfile;
        if File_copy(oldfile, file, true, 1) then
            pop_file_versions fi_- 1 -> pop_file_versions;
            sysdelete(oldfile) ->
        else
            sysunlink(file, false) ->       ;;; error if not accessible
        endif
    endif;
    true
enddefine;

endsection;     /* $-Sys$-Io */



/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1992
        Added optional extra boolean argument follow_symlinks to -sysdelete-
--- John Gibson, Mar 28 1988
        Moved out of sysio.p
 */
