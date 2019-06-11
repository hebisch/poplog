/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/autov55.p
 >  Purpose:        detect v55-ness in an otherwise v200-like terminal
 >  Author:         Tom Khabaza, Oct 12 1984 (see revisions)
 >  Documentation:  HELP * AUTOV55
 */

/*
 *  AUTOV55.P - detect v55-ness in an otherwise v200-like terminal
 *  should be loaded in vedinit.p if a v55 might be used.
 *  adapted without major change from Aaron Sloman's vedinit.p
 *  Tom Khabaza, 12th October 1984
 *
 *  Note that this uses the v55 "answerback" facility - this means
 *  that it will not work on a new v55 until the answerback has been
 *  set up right - to do this (permanently) to a new terminal, use the
 *  following key sequence:
 *      <set up> <shift A> / <ESC> [ 5 5 V / <shift S>
 *  this will leave you in normal (non-setup) mode, and has changed
 *  the NON-VOLATILE memory to know the new answerback sequence.
 */

section;

global vars procedure (vedv55init vedvt52default vedvt52select);

if isundef(vedv55init) then
    veduseterm(% "vi55" %) -> vedv55init;
endif;
if isundef(vedvt52default) then
    veduseterm(% "vi200" %) -> vedvt52default;
endif;

;;; Set -vedterminalselect- to query for a vt52-type terminal
unless islist(vedterminalselect) then [] -> vedterminalselect endunless;
[^^vedterminalselect ^vedtermidseq ['/K' vedvt52select]] -> vedterminalselect;

define vars procedure vedvt52select();
    ;;; test for Visual 55. Assumes usual VT52 terminal ID.
    ;;; also assumes V55 'answer back' has been set to '<ESC>[55V'
    ;;; (Press SET UP. Press SHIFT-A. type '/<ESC>[55V/')
    dlocal vvedscreensendidseq, vedterminalselect;
    returnif(vedterminalname == "vi55" or vedterminalname == "v55");
    rawcharout(%`\^E`%) -> vvedscreensendidseq;
    [['[55V' vedv55init] [^false vedvt52default]] -> vedterminalselect;
    vedinitterm();
    identfn -> vedvt52select;
    syscancel("vedv55init");
    syscancel("vedvt52default");
enddefine;

global vars autov55 = true;         ;;; for -uses-

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Williams, Aug  5 1992
        Added declaration of -autov55- for -uses-.
--- Rob Duncan, Jan 17 1990
        Changed -vedv55init- and -vedvt52default- to be closures of
        -veduseterm- rather than -loadlib-;
        replaced call of -v*edtermsetup- with -vedinitterm-
--- Rob Duncan, Oct 19 1989
        Added top-level assignment to -vedterminalselect-, since it's now
        <false> by default.
        Set -vedvt52default- to load LIB VEDVI200 specifically, since it's
        no longer the default.
--- Jason Handby, Jul 20 1989
        Changed library name from "v55" to "vedvi55" in default -vedvi55init-
--- John Williams, Apr 14 1986
        Made to work if loaded when POPDEFINECONSTANT is TRUE (by
        making VEDVT52SELECT a 'vars' procedure).
--- Aaron Sloman, Mar 28 1985 - Users may define two procedures
    * vedv55init - to be run if terminal is recognised as a V55
        default merely loads lib v55
    * vedvt52default - to be run if not recognised - so its a
        different vt52 emulator, e.g. V200.
        default does nothing
*/
