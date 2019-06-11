/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedxgotomouse.p
 > Purpose:         Move text cursor to mouse cursor in VED
 > Author:          Ian Rogers, Feb  6 1990 (Originally Roger Evans)(see revisions)
 > Documentation:   Below (for now)
 > Related Files:
 */

/*
IMPORTANT NOTE:
    This library is not guaranteed to work with all versions of xterm.
    It may later be replaced with something more sophisticated and robust.
*/

/*  LIB vedxgotomouse                   R.Evans  August 1989

    vedxgotomouse();

    This procedure enables reporting of row/col position when the left mouse
    button is pressed in an xterm, and sets ved's response to be to jump
    the ved cursor to where the mouse cursor is.

    Action is taken depending on whether the location is on the status
    line, or in the VED file, and depending on which mouse button was
    pressed.

    If the library is loaded from inside Ved, the procedure is run
    automatically.

Extended by A. Sloman, Oct 1990
It now has the following features:

There are six user-definable procedures called with line and column
arguments, in the following six contexts. Default versions are provided
as indicated.

Click mouse button 1:
    In file: xmouse1infile(line,column)
        default: vedjumpto(line,column)

    On status line: xmouse1onstatus(line,column)
        default: vedjumpto(line,column), but on status line

Click mouse button 2:
    In file: xmouse2infile(line,column)
        default: move text cursor and mark beginning of range

    On status line: xmouse2onstatus(line,column)
        default: move text cursor and call vedenter

Click mouse button 3:
    In file: xmouse3infile(line,column)
        default: move text cursor and mark end of range

    On status line: xmouse3onstatus(line,column)
        default: move text cursor and call vedredocommand

NOTE: the line and column coordinates are relative to the VED
buffer. On the status line line = vedline as there is only one
line visible. If the position pointed at is on the status line
then true -> ved_on_status is done if necessary before the user procedure
is called, so if it is to do something different from the default
it may have to undo this.
*/

section;

/*  sequence to turn on row/column reporting on mouse press in xterm */
lconstant mouse_enable = '\^[[?9h';

/*  sequence reported by mouse (followed by chars for row+32, col+32,
    which are read by vedx_mouse_goto.
    The final character is (ascii) button_no+32.
*/
lconstant

    vedx_mouse_left = '\^[[M ',
    vedx_mouse_middle = '\^[[M!',
    vedx_mouse_right = '\^[[M"';



;;; USER DEFINABLE PROCEDURES

;;; Procedures to call on button 1 (left)

define vars procedure xmouse1infile(/*line,col*/);
    vedjumpto(/*line,col*/);
enddefine;

define vars procedure xmouse1onstatus(/*line,col*/);
    vedjumpto(/*line, col*/)
enddefine;

;;; Procedures to call on button 2
define vars procedure xmouse2infile(/*line,col*/);
    vedjumpto(/*line,col*/);
    vedmarklo()
enddefine;

define vars procedure xmouse2onstatus(/*line,col*/);
    vedjumpto(/*line, col*/);
    vedenter()
enddefine;

;;; Procedures to call on button 3
define vars procedure xmouse3infile(/*line,col*/);
    vedjumpto(/*line,col*/);
    vedmarkhi()
enddefine;

define vars procedure xmouse3onstatus(/*line,col*/);
    vedjumpto(/*line, col*/);
    vedredocommand();
enddefine;


/*
Procedure to read coordinates, switching buffer if necessary,
then call user defined procedures
*/

define lconstant vedx_mouse_goto(button);
    ;;; action1 is done on status line, action2 elsewhere
    ;;; NB 'x' represents screen column 'y' screenline. wy windowline
    lvars x, y, wy, button, action1, action2;

    vedinascii() - 32 -> x;
    vedinascii() - 32 -> y;

    if button == 1 then
        $-xmouse1infile, $-xmouse1onstatus
    elseif button == 2 then
        $-xmouse2infile, $-xmouse2onstatus
    else
        $-xmouse3infile, $-xmouse3onstatus
    endif -> action1 -> action2;

    if ved_on_status then vedstatusswitch() endif;

    y - vedscreenoffset -> wy;
    if wy >= 1 and wy <= vedwindowlength then
        /* in current window */
    elseif length(vedbufferlist) > 1 then
        vedswapfiles();
        y - vedscreenoffset -> wy;
    else
        ;;; Not in region of VED buffer
        return;
    endif;

    ;;; Now call user procedures with vedline, vedcolumn args
    if wy == 1 then
        ;;; User pointed at status line
        true -> ved_on_status;
        action1(
            vedline, max(x+vedcolumnoffset-1-vedstatusheaderlen, 1));
    else
        false -> ved_on_status;
        action2(max(wy+vedlineoffset-1,1),max(x+vedcolumnoffset-1,1));
    endif
enddefine;

/* procedure to enable everything */
define global vedxgotomouse;
    appdata(mouse_enable, vedscr_char_out);
    vedscr_flush_output();
    vedsetkey(vedx_mouse_left, vedx_mouse_goto(%1%));
    vedsetkey(vedx_mouse_middle, vedx_mouse_goto(%2%));
    vedsetkey(vedx_mouse_right, vedx_mouse_goto(%3%));

enddefine;

/* run it now if already in VED */
#_IF pop_runtime
if vedediting then vedxgotomouse() endif;
#_ENDIF

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  3 1991
        Uses vedscr_ procedures
--- Aaron Sloman, Nov 16 1990
    Fixed bug when on vedstatus
    Moved into $popvedlib
--- Aaron Sloman, Oct  3 1990
    Generalised to cope with status line, and allow user definable
    options for six different actions.
--- Aaron Sloman, Jun 19 1990
    add marklo and markhi options for middle and right buttons.
--- Roger Evans, Jun  1 1990 moved to compat directory
 */
