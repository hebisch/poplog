/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/auto/sysfilesize.p
 >  Purpose:        Find size of file in bytes.
 >  Author:         Updated by Tom Khabaza, Jun 26 1985 (see revisions)
 >  Documentation:  HELP * SYSFILESIZE, *SYS_FILE_STAT
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define sysfilesize(file);
    lvars file;
    lconstant buff = writeable initv(1);
    if sys_file_stat(file, buff) then
        subscrv(1, buff);
    else
        mishap(file, 1, '%CAN\'T GET SIZE OF FILE (%M)');
    endif;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Sep 11 1996
        Removed use of sys*iomessge
--- John Williams, Apr 26 1993
        Made buff writeable, also removed checks on file which are now done
        by sys_file_stat.
 */
