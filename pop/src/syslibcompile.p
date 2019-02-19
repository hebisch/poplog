/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/syslibcompile.p
 > Purpose:         Compiling library files
 > Author:          John Gibson, Mar  7 1988 (see revisions)
 > Documentation:   REF *LIBRARY
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'dirs.ph'

constant
        procedure (subsystem_libcompile)
    ;


;;; ----------------------------------------------------------------------

vars
    popdisk         = POPDISK,
    poplibdir       = POPAUTOLIB,
    procedure prautoloadwarn = erase,
    ;

    /*  popautolist declared incremental in declare.ph.
        This is the basic initialisation.
    */

[%  POPLOCALAUTO, POPHOSTLIB, POPAUTOLIB,
    POPVEDLIB, VEDTERMLIB,
    POPDATABASE
%] -> popautolist;


#_IF DEF SUN
vars popsunlib = POPHOSTLIB;
#_ENDIF


define syslibcompile(name);
    lvars name, lib_list;
    if islist(name) then
        (), name -> (name, lib_list)
    else
        popautolist -> lib_list
    endif;
    ;;; This tests caller(1) == syslibcompile
    subsystem_libcompile(name, lib_list)
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Williams, Apr 23 1993
        Removed POPTURTLE ($usepop/pop/lib/turtle) from $popautolist
--- John Gibson, Jan 11 1993
        Now uses subsystem_libcompile
--- John Gibson, Oct 29 1992
        Added incremental declaration for popautolist
--- John Gibson, Oct 27 1992
        Made syslibcompile clear all but POP11_SLC_PROPAGATE in
        pop_pop11_flags
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- John Gibson, Jul 31 1991
        Changed last arg to sysopen to `A` instead of `D` (in VMS,
        stops mishaps when -name- contains illegal filename chars).
--- John Williams, Mar 21 1991
        -syslibcompile- now complains if library is protected
        (fixes BR isl-er.58).
--- John Williams, Oct 31 1990
        -sysinitcomp- now treats 'init.p' and 'initx.p' consistently.
--- Aaron Sloman, Oct  7 1990
        Altered guard on initx.p
--- Aaron Sloman, Sep 22 1990
        Altered sysinitcomp to compile $poplib/initx.p
--- John Gibson, May 16 1990
        Improved -syslibcompile- to allow idents to contain further idents,
        strings or procedures as well as lists.
--- John Williams, May 15 1990
        -syslibcompile- now allows an ident in the search list
--- Rob Duncan, Oct 25 1989
        Added VEDTERMLIB to -popliblist-
--- John Gibson, Aug  3 1989
        -syslibcompile- now uses -sys_fname_extn-.
--- John Gibson, Jun  2 1989
        Replaced -pop_args_warning- with flag in -pop_pop11_flags-.
--- John Williams, Jun  1 1989
        Added POPHOSTLIB and -popsunlib-
--- John Gibson, May  4 1989
        Made -pop_vm_flags- dlocally 0 in -syslibcompile-.
 */
