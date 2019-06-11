/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvi500screen.p
 >  Purpose:        Conversion of VED for Visual 500 with VT52 emulation.
 >  Author:         A Sloman, Jan 1984 (see revisions)
 >  Documentation:  HELP * V500
 >  Related Files:
 */
compile_mode :pop11 +strict;

uses vedvi200screen;

section;

define lconstant setvi500screen();
    rawcharout(`\^X`);      ;;; make sure not in graphic mode
    vedscreenescape(`3`);   ;;; set high intensity
    rawoutflush();
enddefine;

define global vedvi500screen();

    vedvi200screen();
    setvi500screen();
    "vi500" -> vedterminalname;
    ;;; vt100 translation for VED standard graphic symbols
    vedvt100screengraphtrans -> vedscreengraphtrans;

    33 -> vedscreenlength;

    vedset screen
        setpad      = esc =
        resetpad    = esc >
    endvedset

enddefine;


#_IF not(DEF POPC_COMPILING)

/* WARNING: SEE NOTICE IN LIB *GRAPHIC */
uses graphic;

define setvdu;
    if inscopemode then
        .setalpha;          ;;; set to alphagraphic
        setvi500screen();
        rawoutflush();
        gcucharout ->cucharout;
    endif;
    false ->inscopemode;
enddefine;

#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  8 1992
        Added assignment to vedscreengraphtrans and removed setting of
        graph char variables
--- Jason Handby, Sep 14 1989
        Moved out of old "v500.p" and adapted to use vedset notation
*/
