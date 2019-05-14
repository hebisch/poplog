/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/auto/loadlib.p
 >  Purpose:        Compile a library file from directory in -popuseslist-
 >  Author:         Steve Hardy (see revisions)
 >  Documentation:  HELP * LOADLIB
 >  Related Files:  LIB * USES, LIB * LIB
 */
compile_mode :pop11 +strict;

section;

define vars loadlib(name);
    lvars   name, liblist = popuseslist;
    dlocal  subsystem_compile_warn = erase;
    if islist(name) then (), name -> (name, liblist) endif;
    unless subsystem_libcompile(name, liblist) then
        mishap(name, 1, 'LIBRARY FILE NOT FOUND')
    endunless
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 11 1993
        Subsystem version now the default
--- John Gibson, Nov  9 1992
        Takes optional liblist arg
--- John Gibson, Oct  8 1992
        Took out unnecessary test for popuseslist, which is autoloadable!)
--- Jonathan Meyer, Sep  2 1991 Added compile_mode +strict;
--- Aaron Sloman, May 27 1990  - corrected unless syntax
--- John Williams, Mar  8 1989 - uses -isdefined- instead of -identprops-
--- John Williams, Sep  3 1986 - added 'vars' to define line (for Clisp)
*/
