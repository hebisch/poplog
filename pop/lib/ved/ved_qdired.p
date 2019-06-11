/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_qdired.p
 > Purpose:        See * LIB VED_DIRED    Quits dired
 > Author:         Aaron Sloman, Oct 1 1988 (see revisions)
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

uses ved_dired;

;;; ved_qdired is like ved_dired, but quits file, possibly after
;;; getting new file name from VED buffer.

section;
global constant procedure ved_qdired;
endsection;

section $-dired => ved_qdired;

define ved_qdired =
    ;;; like ved_dired, but quits file after working out what to do
    dired_control(%true%)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, May  1 1989
    removed pdr_valof, and stuff relevant to earlier versions
 */
