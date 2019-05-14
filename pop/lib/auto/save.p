/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/save.p
 >  Purpose:        save output from running a procedure in a named file.
 >  Author:         A.Sloman, 1978
 >  Documentation:  HELP * SAVE, * RECORD
 >  Related Files:  LIB * RECORD
 */
compile_mode :pop11 +strict;

section;

define global vars save(file, proc);
    lvars file proc;
    dlocal cucharout = discout(file);    ;;; re-direct output to file
    proc();
    pr(newline);
    cucharout(termin)
enddefine;

endsection;
