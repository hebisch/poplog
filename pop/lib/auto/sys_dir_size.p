/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.unix/lib/auto/sys_dir_size.p
 > Purpose:         Measure disk space used by directory
 > Author:          John Williams, Jan 22 1992 (see revisions)
 > Documentation:   REF *sys_dir_size
 > Related Files:   LIB * SYS_FILE_MATCH
 */

compile_mode :pop11 +strict;

section;

lconstant
    Statvec =   initv(8),
    NBYTES  =   1,
    NLINKS  =   6,
    INODE   =   8,
    ;

define global sys_dir_size(dir);
    lvars procedure (counted files), n, stat, file, i;
    newanyproperty([], 64, 1, 64, false, false, true, false, false)
        -> counted;
    0 -> n;
    copy(Statvec) -> stat;
    sys_file_match(dir dir_>< '.../*', false, stat, false) -> files;
    until (files() ->> file) == termin do
        -> stat;
        nextunless(stat);
        if fast_subscrv(NLINKS, stat) fi_> 1 then
            fast_subscrv(INODE, stat) -> i;
            nextif(counted(i));
            true -> counted(i)
        endif;
        fast_subscrv(NBYTES, stat) + n -> n
    enduntil;
    n >> 10
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Nov 30 1993
        Now does ">> 10" instead of "div 1000".
--- John Williams, Apr 30 1993
        Now checks that stat isn't false.
--- John Williams, Aug  7 1992
        Made sys_dir_size global.
 */
