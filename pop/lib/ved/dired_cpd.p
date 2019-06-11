/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_cpd.p
 > Purpose:        Used with VED_DIRED to copy a file in same directory
 > Author:         Aaron Sloman 16 April 1990
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED *DIRED_MVD
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
dired_cpd is associated with flag -cpd
    See LIB * DIRED_MVD for details
*/

uses dired_mvd

section;

;;; Necessary for autoloading, if dired_mvd not already loaded.
global vars procedure dired_cpd = dired_mvd;

endsection;
