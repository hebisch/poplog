/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/newvedfileprop.p
 >  Purpose:        create a new property for associating with ved buffers.
 >  Author:         Roger Evans, 1983 (see revisions)
 >  Documentation:  REF *VEDPROCS
 >  Related Files:  LIB * ISVEDFILEPROP
 */
compile_mode :pop11 +strict;

/*
NEWVEDFILEPROP is a procedure of no arguments which returns a new
'vedfileprop' ie a procedure (and updater) which returns/updates a
different value depending on which vedbuffer is current.
*/

section;

define newvedfileprop() -> fileprop;
    ;;; need to access vars through sys_current_val for POPC
    lvars fileprop = sys_current_val("nonactive $-ved_current_file")
                            <> newproperty([], 8, undef, false);

    true -> sys_current_val("ident $-isvedfileprop")(fileprop);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 28 1992
        Moved prop definition to isvedfileprop.p
--- John Gibson, Nov 13 1992
        Changed to make fileprop be ved_current_file <> prop, and to use
        sys_current_val for POPC etc.
--- John Gibson, Apr  8 1992
        Cleaned up and simplified
--- A Sloman, Sep 1986
        Tidied up
*/
