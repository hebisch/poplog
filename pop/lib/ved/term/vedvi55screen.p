/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvi55screen.p
 > Purpose:         VED: Screen control for Visual 55 (in vi200 mode)
 > Author:          Rob Duncan, Oct 19 1989
 > Documentation:
 > Related Files:   LIB * VEDVI55, * VEDVI55KEYS
 */
compile_mode :pop11 +strict;

uses vedvi200screen;

section;

define lconstant setscrollregion(top, bottom) with_props vedsetscrollregion;
    lvars top, bottom;
    vedoutascii('\^[\^_');
    vedoutascii(top + `@`);
    vedoutascii(bottom + `@`);
    1000 ->> vedscreenline -> vedscreencolumn;
enddefine;

define vedvi55screen();
    vedvi200screen();
    "vi55" -> vedterminalname;
    setscrollregion -> vedsetscrollregion;
    vedset screen;
        scrolldown  = esc I
        setpad      = esc =
        resetpad    = esc >
    endvedset;
enddefine;

endsection;
