/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/vedmidwindow.p
 > Purpose:         Puts the current line in the middle of the window
 > Author:          Aaron Sloman, Oct 1983 (see revisions)
 > Documentation:   HELP * VEDPROCS /vedmidwindow
 > Related Files:
 */

section;

define global vedmidwindow();
    lvars offs = max(0, vedline - (vedwindowlength >> 1));
    unless vedlineoffset == offs then
        offs -> vedlineoffset;
        vedrefresh();
    endunless;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Andreas Schoter, 6 Sept 1989
    Rewrote to use the Roger Evans version from VEDXTERMCORE
--- Mark Rubinstein, Nov 13 1985 - sectionised and lvarsed.
 */
