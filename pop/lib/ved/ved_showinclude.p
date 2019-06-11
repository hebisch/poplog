/*  --- Copyright University of Sussex 1996. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_showinclude.p
 >  Purpose:        for examining library files
 >  Author:         Aaron Sloman (Sept 1990) (see revisions)
 >  Documentation:  REF * VED_SHOWINCLUDE, * INCLUDE, REF *POPINCLUDELIST,
                    HELP * INCLUDE HELP *SHOWLIB,  REF * LIBRARY
 >  Related Files:  LIB * INCLUDE , * POPINCLUDELIST
 */
compile_mode :pop11 +strict;

section;

define vars ved_showinclude();
    lvars n, arg = vedargument, place = nullstring;
    vedsetup();
    if strmember(`\s`, arg) ->> n then
        n-1 -> n;
        allbutfirst(n, arg) -> place;
        substring(1, n, arg) -> arg
    endif;
    if arg /= nullstring and sys_fname_extn(arg) = nullstring then
        ;;; Add explicit include-file extension
        ;;; Use '.ph' if filename is all lower or mixed case, '.PH' otherwise
        ;;;     (because of case-conversion done subsequently by vedsysfile)
        lvars char, extn = '.PH';
        for char in_string arg do
            if islowercode(char) then
                '.ph' -> extn;
                quitloop
            endif
        endfor;
        arg <> extn -> arg
    endif;
    arg <> place -> vedargument;
    vedsysfile("vedlibname",
                popincludelist,
                procedure();
                    "include" -> vedfileprops;
                    vedhelpdefaults();
                endprocedure);
enddefine;

"ved_showinclude" -> vedgetsysfilepdr("INCLUDE");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 12 1996
        Made it cope with a vvedgotoplace
--- John Williams, Sep 14 1992
        Now adds '.ph' in uppercase if passed an all uppercase file name
--- Robert John Duncan, Feb 10 1992
        Removed dlocal of -pop_default_type- which doesn't get undone if
        this is used as the entry point to ved. Added an explicit '.ph'
        extension to the argument instead.
--- John Gibson, Oct 31 1990
        Moved -showinclude- to where it should have been, i.e. in
        $popautolib
 */
