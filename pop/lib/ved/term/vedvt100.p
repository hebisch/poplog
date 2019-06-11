/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt100.p
 > Purpose:         Co-ordinate the use of vedvt100key and vedvt100screen
 > Author:          Andreas Schoter (see revisions)
 > Related Files:   vedvt100keys.p vedvt100screen.p
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; because already defined in core system

uses-by_name vedvt100screen, vedvt100keys;

section;

;;; Unprotect -vedvt100- in case this is loaded in a system with it
;;; built in
sysunprotect("vedvt100");

define vars vedvt100(dummy);
    lvars dummy;
    veduseterm("vt100") -> ;
    erase -> vedvt100;
enddefine;

if iscaller(vedsetup) then vedvt100(false) endif;

endsection;

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct  9 1989
        Sectionised
 */
