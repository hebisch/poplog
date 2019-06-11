/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/vedresize.p
 > Purpose:         Adjust VED to new window size
 > Author:          Ben Rubinstein, Mar  6 1986 (see revisions)
 > Documentation:
 > Related Files:
 */

section;

;;; vedresize:
;;;     adjust display to fit window size of nlines x ncols.

define global vedresize(nlines, ncols);
    lvars nlines, ncols, redraw = true, oldscreenlength = vedscreenlength;
    ;;; Check for optional third -redraw- parameter
    if isboolean(ncols) then
        nlines, ncols -> redraw -> ncols -> nlines;
    endif;
    ;;; Check for sensible (?) input
    unless isinteger(ncols) and ncols fi_> 0
    and isinteger(nlines) and nlines fi_> 0
    then
        mishap(nlines, ncols, 2, 'INTEGERS > 0 NEEDED');
    endunless;
    ;;; Set new window size
    nlines -> vedscreenlength;
    ncols -> vedscreenwidth;
    if vedscreenwrap then
        vedscreenwidth - 1 -> vedscreenwidth;
    endif;
    ;;; Adjust pop line length to stop horrible scrolling in VED output
    vedscreenwidth - 2 -> poplinemax;
    (7 * vedscreenwidth) >> 3 -> poplinewidth;  ;;; Ie. 7/8 of width
    ;;; Rebuild status line strings and offsets in all buffers
    vedappfiles(procedure;
        initdstring(vedscreenwidth) -> vedstatusline;
        if vedscreenoffset fi_> 0 then
            ;;; in lower window
            vedscreenlength fi_- vedstartwindow -> vedscreenoffset;
            vedstartwindow -> vedwindowlength;
        elseif vedwindowlength == oldscreenlength then
            ;;; full screen
            vedscreenlength -> vedwindowlength;
        else
            ;;; in upper window
            vedscreenlength fi_- vedstartwindow -> vedwindowlength;
        endif;
    endprocedure);
    ved_save_file_globals();
    if redraw then
        ;;; Refresh current display
        vedscr_flush_output();
        vedrestorewindows();
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  9 1996
        Made setting of poplinemax/width be the same as for XVed.
--- John Gibson, Sep 19 1995
        Fixed to use initdstring instead of inits when remaking vedstatusline
--- John Gibson, Jun 23 1991
        Uses ved_save_file_globals
--- John Gibson, Jun  3 1991
        Uses vedscr_flush_output
--- Rob Duncan, Feb  1 1990
        Renamed from -vedwin_adjust-.
        Added: optional -redraw- parameter; check for -vedscreenwrap- before
        reducing the screen width; mishap on non-integer input.
        Removed assignment to -vedstartwindow- (now adjusted automatically
        by the system).
        Fixed a bug in determining window size and offset for each file.
--- John Gibson, Nov 11 1987
        Replaced -popdevraw- with -poprawdevout-
--- Robert Duncan, Sep  1 1987
        changed the assignment to -poplinemax- to leave 3 characters free
        so that a full line of LMR or IM output can fit without the screen
        having to be redrawn. See bugreport robd.1
 */
