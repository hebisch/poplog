/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:            C.all/lib/ved/term/vedwinsun.p
 > Purpose:         Configure VED for a SUN workstation runnning sunview
 > Author:          Ben Rubinstein, Mar  6 1986 (see revisions)
 > Documentation:   HELP * VEDWINSUN
 > Related Files:   LIB * VEDWINSUNPROX
 */
compile_mode :pop11 +strict;

uses vedsun;

section;

;;; vedwinsun:
;;;     same as -vedsun-, but in a sunview window

define vars vedwinsun();
    dlocal vedsuninwindow = true;
    vedsun();
enddefine;

if iscaller(vedsetup) then vedwinsun() endif;

endsection;

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Nov  2 1989
        Added "dlocal vedsuninwindow"
--- Rob Duncan, Oct 26 1989
        Merged with LIB * VEDSUN: code for resizing windows etc. moved
        into "vedsunscreen.p"
--- Rob Duncan, Oct 18 1989 -
        changed to use -vvedscreeninit- instead of -vedinit-.
        Made -vedwinsun- a procedure and added conditional call at
        the end to preserve consistency with other libraries.
--- Ben Rubinstein, Feb 25 1987 - added necessary load of -popsunlib-
*/
