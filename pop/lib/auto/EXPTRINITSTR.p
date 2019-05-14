/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/EXPTRINITSTR.p
 > Purpose:         Macro to produce exptr to fixed struct of size for type
 > Author:          John Gibson, Sep 21 1991 (see revisions)
 > Documentation:   REF *DEFSTRUCT
 */
compile_mode:pop11 +strict;

section;

sysunprotect("EXPTRINITSTR");

define :inline global EXPTRINITSTR(type=typespec);
    initexptr_mem(SIZEOFTYPE(type))
enddefine;

sysprotect("EXPTRINITSTR");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  4 1992
        Changed to use initexptr_mem
 */
