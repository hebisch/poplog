/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:           C.all/lib/auto/sysmodtime.p
 > Purpose:        Time of last modification of file.
 > Author:         Tom Khabaza, Jun 26 1985 (see revisions)
 > Documentation:  HELP * SYSMODTIME
 > Related Files:  HELP * SYS_FILE_STAT
 */
compile_mode :pop11 +strict;

section;

define sysmodtime(file);
    lvars file;
    lconstant buff = writeable initv(2);
    if sys_file_stat(file, buff) then
        subscrv(2, buff);
    else
        mishap(file, 1, '%CAN\'T GET MODIFICATION TIME OF FILE (%M)');
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 11 1996
        Removed use of sys*iomessage
--- John Williams, Apr 26 1993
        Made buff writeable, also removed checks on file that are now
        performed by sys_file_stat.
 */
