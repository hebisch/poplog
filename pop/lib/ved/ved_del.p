/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_del.p
 > Purpose:         Prepare to delete files marked with a '*'
 > Author:          Aaron Sloman, Dec 27 1983 (see revisions)
 > Documentation:   HELP * PURGE
 > Related Files:   LIB * VED_SHELL (Unix) and LIB * VED_DODCL (VMS)
 */

/*
To be used inside a VED directory listing produced by <ENTER> PURGE.

Assumes that the user that the user has inserted '*' at the beginning of
each line that names a file to be deleted.

Then <ENTER> DEL removes all lines that don't start with '*', and
prefixes those that do with either 'rm' (Unix) or 'DELETE' (VMS).

Finally, the file deletion commands can be executed by doing either
<ENTER> SHELL (Unix) or <ENTER> DODCL (VMS).
*/

section;

lconstant VMS = hd(sys_os_type) == "vms";

#_IF VMS

define lconstant Insert_del_command();
    dlocal vedautowrite = false, vedbreak = false;
    ;;; Deletes text after the first text item on the line
    ;;;  (which is assumed to be a filename)
    until vedcolumn > vvedlinesize then
        if vedcurrentchar() == `\s` then
            vedcleartail();
            quitloop()
        endif;
        vedcharright()
    enduntil;
    vedscreenleft();
    vedinsertstring('DELETE ')
enddefine;

#_ELSE /* Unix */

define lconstant Insert_del_command();
    lvars dir;
    dlocal vedautowrite = false, vedbreak = false;
    ;;; Deletes text before the last text item on the line
    ;;;  (which is assumed to be a filename)
    vedcurrentchar() == `d` -> dir;
    vedtextright(), vedcharleft();
    until vedcolumn == 1 do
        if vedcurrentchar() == `\s` then
            vedcharright();
            vedclearhead();
            quitloop
        endif;
        vedcharleft()
    enduntil;
    vedinsertstring('rm ');
    if dir then vedinsertstring('-r ') endif
enddefine;

#_ENDIF

define global ved_del();
    unless
#_IF VMS
    issubstring('mba', vedpathname)
#_ELSE
    isendstring('.ls', vedfileprops)
#_ENDIF
    do
        vederror('NOT A DIRECTORY LISTING')
    endunless;
    vedtopfile();
    until vedatend() do
        if vvedlinesize == 0
        or vedthisline()(1) /= `*` then
            vedlinedelete()     ;;; Delete lines that don't start with '*'
        else
            veddotdelete();     ;;; Remove the '*'
            Insert_del_command();
            vednextline()
        endif;
    enduntil;
    false -> vedchanged;        ;;; Prevent file being written accidentally
    vedtopfile();
    if vvedbuffersize > 0 then
        vedputmessage(#_< 'CHECK!!! then type ENTER '
                            <> (if VMS then 'DODCL' else 'SHELL' endif)
                            <> ' to get files deleted'
                        >_#)
    else
        vedputmessage('NO FILES TO BE DELETED?')
    endif;
    vedscreenbell()
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jun 26 1997
        Fixed bug in test for directory listing file.
        Inserts 'rm -r' for directories (Unix only).
--- John Williams, Nov 17 1992
        Merged Unix and VMS versions
 */
