/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 *  File:           C.all/lib/ved/ved_ll.p
 *  Purpose:        make ved work with VT100 in long line mode (132 columns)
 *  Author:         Aaron Sloman, July 1982 (see revisions)
 *  Documentation:  HELP * VEDCOMMS
 */
compile_mode :pop11 +strict;

;;; This library package used to assume that only 14 lines were available in
;;; 132 column mode - a restriction that applied to poor terminal emulations
;;; and VT100's without the "Advanced Video Option" fitted.  It has now been
;;; changed so that the restriction is assumed not to apply unless the
;;; variable "vedvt100noavo" is true.

section;

vars vedvt100noavo;
if isundef(vedvt100noavo) then false -> vedvt100noavo endif;

lvars longlines, oldlen, oldstart;

define vars ved_ll();

    define lconstant setstatusline();
        if longlines then
            if vedvt100noavo then
                ;;; VT100 allows only 14 lines in 132 column mode
                14 ->> vedscreenlength  ->> vedstartwindow -> vedwindowlength;
            endif;
            132 -> vedscreenwidth;
            vedscreenwidth - 2 -> vedlinemax;
        else
            if vedvt100noavo then
                oldlen ->> vedscreenlength -> vedwindowlength;
                oldstart -> vedstartwindow;
            endif;
            78 -> vedlinemax;
            80 -> vedscreenwidth;
        endif;
        if datalength(vedstatusline) /== vedscreenwidth then
            ;;; re-initialise status line if not the right length
            inits(if longlines then 132 else 80 endif) -> vedstatusline
        endif;
    enddefine;

    if isundef(longlines) then
        ;;; first time called
        vedscreenwidth == 132 -> longlines;
        vedscreenlength -> oldlen;
        vedstartwindow -> oldstart;
    endif;

    not(longlines) -> longlines;
    if longlines then
        vedscreencontrol('\^[[?3h');
    else
        vedscreencontrol('\^[[?3l');
    endif;
    ved_save_file_globals();
    vedappfiles(setstatusline);
    vedscr_flush_output();
    if vedediting then
        false ->> vedupperfile -> vedlowerfile;
        vedscreencontrol(vvedscreenclear);
        vedsetonscreen(vedcurrentfile,'line length ' >< vedscreenwidth);
    endif;
enddefine;

endsection;                                 ;;; top-level


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug 31 1993
        Removed execute-level use of vedscreenwidth etc.
--- John Gibson, Jun  3 1991
        Uses vedscr_flush_output
--- Rob Duncan, Oct 27 1989
        Changed -vedscreenescape- to -vedscreencontrol-
--- Mark Rubinstein, Jul 16 1985 - logical correction of initialiastion of
    VEDVTNOAVO and removal of section through use of LVARS and LCONSTANTS.
--- Mark Rubinstein, Jun 18 1985 - altered to allow more than 14 lines
    in long line mode.
 */
