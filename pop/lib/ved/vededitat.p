/*  --- Copyright University of Sussex 1994. All rights reserved. ----------
 >  File:           C.all/lib/ved/vededitat.p
 >  Purpose:        for editing a file starting with cursor at specified line
 >  Author:         Mark Rubinstein, Jun 18 1985 (see revisions)
 >  Documentation:  REF * VEDPROCS, REF * VEDEDITOR
 */
compile_mode :pop11 +strict;

section;

    ;;; file can be a string or a Ved file structure
define vededitat(file, defaults_p, vvedgotoplace);
    lvars file, defaults_p;
    dlocal vvedgotoplace;
    ;;; check for args in old order
    if isprocedure(file) then file, defaults_p -> (defaults_p, file) endif;
    vededit(file, defaults_p);
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Mar  7 1994
        Changed to use vededit
--- Aaron Sloman, Sep  9 1990
        changed "vars" to dlocal. Added comment
 */
