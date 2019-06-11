/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_peek.p
 > Purpose:        With LIB VED_DIRED, peek at a file.
 > Author:         Aaron Sloman, Oct 1 1988 (see revisions)
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
dired_peek is associated with flag -peek in ved_dired
*/

uses ved_dired;

section $-dired => dired_peek;

define global dired_peek(flag,file,dummy, quit_first);
    ;;; Read the first VEDSTARTWINDOW -2 lines of file into a VED window
    ;;; then offer a menu of options
    ;;; the 'flag' and 'dummy' arguments are not used
    lvars flag, file, dummy, quit_first, dev, n;
    lconstant menustring=
        'q=Quit file, l=Leave it, v=VED it, d= delete, m=more';

    dlocal vedediting;

    if sysopen(file,0,"line") ->> dev then
        vededit(dired_tempfile(file), vedhelpdefaults);
        false -> vedbreak;
        vedinsertstring(dired_header);
        vedinsertstring(vedcommand); vedcharright(); vedinsertstring(file);
        repeat
            false -> vedediting;
            repeat vedstartwindow - 2 times
                sysread(dev,sysstring,sysstringlen) -> n;
            quitif(n == 0);
                vedlinebelow();
                substring(1,n-1,sysstring) -> vedthisline();
            endrepeat;
            true -> vedediting;
            -500 -> vedlineoffset;  ;;; force a refresh in vedcheck
            vedcheck();
        quitif(n == 0);
            vedputmessage(menustring);
            vedwiggle(0,vedlinemax);
            vedinascii() -> n;
            if n == `q` then ved_q(); dired_setup()
            elseif n == `l` then ;;; leave the file
            elseif n == `v` then file -> vedargument; ved_qved();
            elseif n == `d` then dired_rm(false,file,false); ved_q();
            else nextloop
            endif;
            return();
        endrepeat
    else vederror(NOFILE sys_>< file)
    endif
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  5 1992
        Made -dired_peek- global; also fixed missing semi-colon on line 45
--- Poplog System, Nov 29 1988
        Changed "dired_delete" to "dired_rm" (cf ALPHA 8)
 */
