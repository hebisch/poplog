/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.unix/lib/ved/dired_load.p
 > Purpose:         Compile file named on current line
 > Author:          Aaron Sloman, Mar 23 1989 (see revisions)
 > Documentation:   HELP * DIRED
 > Related Files:   LIB * VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

uses ved_dired;

section $-dired => dired_load;

define global dired_load(flag,file,dummy,quit);
    lvars dev, flag, file, dummy, quit;
    dlocal subsystem_compile_warn = erase;
    if readable(file) ->> dev then
        subsystem_compile(dev, false)
    else
        vederror('NO SUCH FILE as ' >< file)
    endif
enddefine;

;;; Do something like this to map it onto a key
;;; vedsetkey('\^Xl',
;;;     procedure; vars vedargument = '-load'; ved_dired() endprocedure);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1993
        Changed to use subsystem_compile
--- John Williams, Aug  5 1992
        Made -dired_load- global
--- Aaron Sloman, May 14 1990
        Made selection of compiler sensitive to file extension
 */
