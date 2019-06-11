/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/vedkey5.p
 >  Purpose:        alter the function of the 5 key on the keypad.
 >  Author:         Chris Slymon, May 1983 (see revisions)
 >  Documentation:  HELP * VEDKEY5
 >  Related Files:
 */

;;; Keypad 5 altered so that when it precedes one of keypad 2,4,6 or 8
;;; the window scrolls in such a way that the cursor moves across the screen
;;; e.g. a third of the window up, but remains in the same place relative
;;; to the file.

;;; E.g. if cursor is on bottom line of window, keypad 5 then 8 scrolls the
;;; window, so as to see higher up the file. This is similar to
;;; PUSH VEDCHARUPLOTS POP but without affecting the position stack

;;; Note that keypad keys not preceded by keypad 5 are not affected

section;

define lconstant veddoscrollup;
    lvars num;
    if vedlineoffset > vvedbuffersize - 2 then
        vederror('\{b}end of file');
    elseif vedline == vedlineoffset + 1 then
        vederror('\{b}top of window');
    else
        vedwindowlength >> 2 -> num;   ;;; divide by 4
        while num > 0 and
            vedlineoffset <= vvedbuffersize - 2 and
            vedline > vedlineoffset + 1 do
            vedscrollvert(1);
            num - 1 -> num;
        endwhile;
    endif
enddefine;

define lconstant veddoscrolldown;
    lvars num;
    if vedlineoffset == 0 then
        vederror('\{b}top of file');
    elseif vedline == vedlineoffset + vedwindowlength - 1 then
        vederror('\{b}bottom of window');
    else
        vedwindowlength >> 2 -> num;
        while num > 0 and
            vedlineoffset > 0 and
            vedline < vedlineoffset + vedwindowlength - 1 do
            vedscrollvert(-1);
            num - 1 -> num;
        endwhile;
    endif
enddefine;

define lconstant veddoscrollright;
    lvars num;
    if vedcolumnoffset == 0 then
        vederror('\{b}right side of file');
    elseif vedcolumn == vedcolumnoffset + vedscreenwidth - 1 then
        vederror('\{b}cursor at edge of screen');
    else
        vedscreenwidth >> 3  -> num;   ;;; divide by 8
        while num > 0 and
            vedcolumn < vedcolumnoffset + vedscreenwidth - 1 and
            vedcolumnoffset > 0 do
            vedscrollhorz(-1);
            num - 1 -> num;
        endwhile;
    endif
enddefine;

define lconstant veddoscrollleft;
    lvars num;
    if vedcolumn == vedcolumnoffset + 1 then
        vederror('\{b}cursor at edge of screen');
    else
        vedscreenwidth >> 3 -> num;    ;;; divide by 8
        while num > 0 and
            vedcolumn > vedcolumnoffset + 1 do
            vedscrollhorz(1);
            num - 1 -> num;
        endwhile;
    endif
enddefine;

sys_runtime_apply(
    procedure;
        vedset keys
            doscrolldown        = esc ? u esc ? x
            doscrollleft        = esc ? u esc ? v
            doscrollright       = esc ? u esc ? t
            doscrollup          = esc ? u esc ? r
        endvedset
    endprocedure
);

constant vedkey5 = true;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Dec  1 1992
        Sets keys at runtime
--- Jason Handby, Jul 20 1989
    Changed to use vedset notation, removed redundant test for v55
--- Mark Rubinstein, Nov 13 1985 - lvarsed and lconstantised.
--- Mark Rubinstein, Nov 9 1984 - Altered to also work for v55 terminals
*/
