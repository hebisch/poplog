/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ds.p
 >  Purpose:        delete from current positon up to next occurenc of string
 >  Author:         Aaron Sloman, Dec 1984 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_ds
 >  Related Files:  LIB * VED_CUT,  LIB * VED_SPLICE, LIB * VED_DSB
 */
compile_mode :pop11 +strict;

;;; DS - delete/search                Aaron Sloman Dec 1984

;;; ENTER DS <string>
;;; delete from current position up to but excluding next occurrence of
;;; string.
;;; ENTER DSB <string>
;;; delete from current position back to and including previous occurrence
;;; of string.
;;; ENTER YDS
;;; re-insert last text removed by DS or DSB

section;

define vars ved_ds;
    ;;; search forward to argument, and delete up to it.
    lvars pos;
    dlocal vedpositionstack;
    vedpositionpush(); front(vedpositionstack) -> pos;
    vedlocate(vedargument);
    if vedline < pos(1) then
        vedpositionpop(); vederror(vedargument >< ' NOT FOUND')
    endif;
    vedpositionpush();  ;;; ved_cut needs two stacked positions
    ved_cut();
enddefine;

define vars ved_dsb;
    ;;; search backward to argument, and delete to it.
    dlocal vedpositionstack;
    vedpositionpush();
    vedbacklocate(vedargument);
    vedpositionpush();
    ved_cut();
enddefine;

define vars ved_yds;
    ved_splice();
enddefine

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun  2 1986 added cross references
*/
