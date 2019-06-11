/* --- Copyright University of Sussex 1988.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_help.p
 > Purpose:        Used by VED_DIRED to get help
 > Author:         Aaron Sloman, Oct 1 1988
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
dired_help is associated with flags -?? and -? in ved_dired
*/
uses ved_dired;

section $-dired => dired_help;

define global dired_help(flag,dummy1,dummy2,quit_first);
    lvars flag, dummy1, dummy2, quit_first;
    dlocal vedargument;
    if flag = '-??' then 'dired' else 'dired.short' endif
        -> vedargument;
    vedsysfile("vedhelpname", vedhelplist, false);
    dired_setup();
enddefine;

endsection;
