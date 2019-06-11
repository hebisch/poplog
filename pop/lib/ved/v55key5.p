/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.all/lib/ved/v55key5.p
 > Purpose:        Alter affect of keypad key 5 for V55 terminals
 > Author:         Chris Slymon, May 1983 (see revisions)
 > Documentation:  HELP * VEDKEY5
 > Related Files:  LIB * VEDKEY5
 */

#_TERMIN_IF DEF POPC_COMPILING

;;; Keypad 5 altered so that when it precedes one of keypad 2,4,6 or 8
;;; the window scrolls in such a way that the cursor moves across the screen
;;; e.g. a third of the window up, but remains in the same place relative
;;; to the file.

;;; E.g. if cursor is on bottom line of window, keypad 5 then 8 scrolls the
;;; window, so as to see higher up the file. This is similar to
;;; PUSH VEDCHARUPLOTS POP but without affecting the position stack

;;; Note that keypad keys not preceded by keypad 5 are not affected

section;

define global procedure vednomorescroll;
    vederror('CANNOT SCROLL FURTHER')
enddefine;

define veddoscrollup;
lvars num;
    if vedlineoffset > vvedbuffersize - 2 then
        vednomorescroll();
    elseif vedline == vedlineoffset + 1 then
        vednomorescroll();
    else
        vedwindowlength >> 2 -> num;   ;;; divide by 4
        while num fi_> 0 and
                vedlineoffset fi_<= vvedbuffersize fi_- 2 and
                    vedline fi_> vedlineoffset fi_+ 1 do
            vedscrollup();
            num fi_- 1 -> num;
        endwhile;
    endif
enddefine;

define veddoscrolldown;
lvars num;
    if vedlineoffset == 0 then
        vednomorescroll();
    elseif vedline == vedlineoffset fi_+ vedwindowlength fi_- 1 then
        vednomorescroll();
    else
        vedwindowlength >> 2 -> num;
        while num fi_> 0 and
                vedlineoffset fi_> 0 and
                    vedline fi_< vedlineoffset fi_+ vedwindowlength fi_- 1 do
            vedscrolldown();
            num fi_- 1 -> num;
        endwhile;
    endif
enddefine;

define veddoscrollright;
lvars num;
    if vedcolumnoffset == 0 then
        vednomorescroll();
    elseif vedcolumn == vedcolumnoffset fi_+ vedscreenwidth fi_- 1 then
        vednomorescroll();
    else
        vedscreenwidth >> 3  -> num;   ;;; divide by 8
        max(max(0, vedcolumnoffset fi_- num),
                vedcolumn fi_- vedscreenwidth fi_+ 1) -> vedcolumnoffset;
        vedrefresh();
    endif
enddefine;

define veddoscrollleft;
lvars num;
    if vedcolumn == vedcolumnoffset fi_+ 1 then
        vednomorescroll();
    else
        vedscreenwidth >> 3 -> num;    ;;; divide by 8
        min(vedcolumnoffset fi_+ num, vedcolumn fi_-1) -> vedcolumnoffset;
        vedrefresh();
    endif
enddefine;

vedsetkey('\^[?u\^[?x',  veddoscrolldown);
vedsetkey('\^[?u\^[?v',  veddoscrollleft);
vedsetkey('\^[?u\^[?t',  veddoscrollright);
vedsetkey('\^[?u\^[?r',  veddoscrollup);
vedsetkey('\^[?u\^[?u',  undef);    ;;; not sure why this is needed
                                    ;;; bug in vedsetkey?

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Mar 1985 - Sectionised and converted to V55.  Scrolling left
    and right too slow. So use vedrefresh, alas.
*/
