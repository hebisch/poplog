/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_purge.p
 > Purpose:         Use VED to list and purge files
 > Author:          Aaron Sloman, April 1982 (see revisions)
 > Documentation:   HELP * PURGE
 > Related Files:   LIB * VED_LS (Unix), LIB * VED_DIR (VMS), LIB * VED_DEL
 */

section;

define global ved_purge();
    vedinput(
        procedure();
            vedputcommand(nullstring);
            vedputmessage('Mark unwanted files with * on left, then type ENTER DEL');
            vedscreenbell()
        endprocedure);
#_IF hd(sys_os_type) == "vms"
    veddo('dir/size/date ' <> vedargument);
#_ELSE
    veddo('ls -l ' <> vedargument);
#_ENDIF
enddefine;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Nov 17 1992
        Now uses vedinput, to work with new version of LIB * VED_DIR.
--- Mark Rubinstein, Nov 12 1985
        Made to work on both VMS and Unix.
 */

endsection;
