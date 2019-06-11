/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedciferkeys.p
 > Purpose:         set up the keybindings for cifer terminals
 > Author:          Aled Morris, 13 Dec, 1985 (see revisions)
 > Related Files:   vedcifer.p vedciferscreen.p
 */
compile_mode :pop11 +strict;

uses vedvi200keys;

section;

define vedciferkeys();
    vedvi200keys();

    ;;; closures on -vedscreencontrol-
    lconstant vedmessgline = vedscreencontrol(%'\^[W\^@'%);
    lconstant vedblankmess = vedscreencontrol(%'\^[V\^@'%);

    vedset keys
    ;;; large cursor moves
        chardownleftlots   = esc esc ? q
        chardownlots       = esc esc ? r
        chardownrightlots  = esc esc ? s
        charleftlots       = esc esc ? t
        charmiddle         = esc esc ? u
        charrightlots      = esc esc ? v
        charupleftlots     = esc esc ? w
        charuplots         = esc esc ? x
        charuprightlots    = esc esc ? y

    ;;; effects of blank key (bottom left of keyboard)
        messgline          = esc ? o
        blankmess          = esc ? z

    ;;; effects of SKIP key
        chardelete         = esc ? b
    endvedset;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Andreas Schoter, Jul 18 1989
        Modified to use seperate key and screen set up with vedset
*/
