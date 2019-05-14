/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/auto/needs_prolog.p
 *  Purpose:        POP-11 macro for checking that PROLOG is loaded
 *  Author:         Unknown, ???
 *  Documentation:
 *  Related Files:
 */

section;

define global macro needs_prolog;
    unless identprops("prolog") == "macro"
    and isprocedure(valof("prolog"))
    and isconstant("prolog") do
        mishap(0, 'Prolog system must be loaded')
    endunless
enddefine;

endsection; /* pop_section */
