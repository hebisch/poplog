/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.unix/lib/auto/sysfileinode.p
 > Purpose:        Accessing the inode of a file
 > Author:         A.Sloman, Jan 27 1986 (see revisions)
 > Documentation:  HELP * SYSFILEINODE
 > Related Files:  LIB * SYS_FILE_STAT  HELP * SYS_FILE_STAT
 */

section;

define global procedure sysfileinode(file);
    lvars file;
    lconstant vec = writeable initv(8);
    if sys_file_stat(file, vec) then
        vec(8)
    else
        false
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Apr 26 1993
        Made vec writeable
 */
