/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/vedtopwindow.p
 > Purpose:         Positions the current line at the top of the window
 > Author:          Roger Evans (see revisions)
 */

section;

define global vedtopwindow();
    lvars offs = max(0, vedline - 1);
    unless vedlineoffset == offs then
        offs -> vedlineoffset;
        vedrefresh();
    endunless;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct  9 1989
    Modified not to refresh if already at top.
--- Andreas Schoter, 6 Sept 1989
    Removed from LIB * VEDXTERMCORE and made autoloadable.
 */
