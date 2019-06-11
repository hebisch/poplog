/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt52screengraphtrans.p
 > Purpose:         Translate VED standard graphic codes for vt52
 > Author:          John Gibson, Feb  7 1992
 > Documentation:   REF *VEDTERMINALS
 */
compile_mode :pop11 +strict;

section;

    ;;; Called for graphics chars in the range 16:81 - 16:9D
    ;;; (currently only upto 16:92 are used)
define vedvt52screengraphtrans(char);
    lvars char, c;
    ;;;                123456789ABCDEF0123456789ABCD
    lconstant trans = '```aemcasldanobf#.___________' ;

    fast_subscrs(char fi_- 16:80, trans) -> c;
    ;;; 2nd result true means graphics mode (n.b. # and . don't get
    ;;; interpreted as graphics).
    if c == `_` then char, false else c, true endif
enddefine;

endsection;
