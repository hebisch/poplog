/* --- Copyright University of Birmingham 1993. All rights reserved. ------
 > File:            C.all/lib/ved/vedjoinline.p
 > File:            $poplocal/local/auto/vedjoinline.p
 > Purpose:         Join current line with previous line
 > Author:          Aaron Sloman, Feb  7 1992
 >                      Moved into main system 26 Jul 2003
 > Documentation:   Below
 > Related Files:
 */

/*

This procedure can be mapped onto a key sequence, e.g. ESC DEL. It joins the
current line with the previously line, ignoring vedbreak, vedstatic, etc. The
resulting line may be longer than would normally be possible with vedbreak
true.

It also does not require the cursor first to be put at the beginning of the
line and the DEL key used. It works wherever the cursor is.

*/

section;

define global vedjoinline();
    ;;; join current line with previous line, ignoring vedbreak and vedstatic
    dlocal vedbreak = false, vedstatic = false;
    1 -> vedcolumn;
    vedchardelete()
enddefine;

endsection;
